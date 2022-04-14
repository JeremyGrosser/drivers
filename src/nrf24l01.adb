--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body NRF24L01 is

   procedure SPI_Transfer
      (This : in out Device;
       Data : in out SPI_Data_8b)
   is
      Status : SPI_Status;
   begin
      --  One byte at a time, we don't know how deep the receive FIFO is
      for I in Data'Range loop
         This.Port.Transmit (Data (I .. I), Status, Timeout => 1);
         if Status /= Ok then
            raise Program_Error with "SPI Transmit error";
         end if;
         This.Port.Receive (Data (I .. I), Status, Timeout => 1);
         if Status /= Ok then
            raise Program_Error with "SPI Receive error";
         end if;
      end loop;
   end SPI_Transfer;

   procedure W_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : UInt8_Array)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#0010_0000# or Register'Enum_Rep (Reg));
      Dat : SPI_Data_8b (1 .. Data'Length) := SPI_Data_8b (Data);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.SPI_Transfer (Dat);
      This.CS.Set;
   end W_REGISTER;

   procedure W_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : UInt8)
   is
   begin
      This.W_REGISTER (Reg, UInt8_Array'(1 => Data));
   end W_REGISTER;

   procedure R_REGISTER
      (This : in out Device;
       Reg  : Register;
       Data : out UInt8)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => Register'Enum_Rep (Reg));
      Dat : SPI_Data_8b (1 .. 1) := (1 => 16#FF#);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.SPI_Transfer (Dat);
      This.CS.Set;
      Data := Dat (1);
   end R_REGISTER;

   procedure FLUSH_TX
      (This : in out Device)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#1110_0001#);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.CS.Set;
   end FLUSH_TX;

   procedure FLUSH_RX
      (This : in out Device)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#1110_0010#);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.CS.Set;
   end FLUSH_RX;

   procedure W_TX_PAYLOAD
      (This : in out Device;
       Data : UInt8_Array)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#1010_0000#);
      Dat : SPI_Data_8b (1 .. Data'Length) := SPI_Data_8b (Data);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.SPI_Transfer (Dat);
      This.CS.Set;
   end W_TX_PAYLOAD;

   procedure R_RX_PAYLOAD
      (This : in out Device;
       Data : out UInt8_Array)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#0110_0001#);
      Dat : SPI_Data_8b (1 .. Data'Length) := (others => 16#FF#);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.SPI_Transfer (Dat);
      This.CS.Set;
      Data := UInt8_Array (Dat);
   end R_RX_PAYLOAD;

   procedure NOP
      (This   : in out Device;
       Status : out STATUS_Register)
   is
      Cmd : SPI_Data_8b (1 .. 1) := (1 => 2#1111_1111#);
   begin
      This.CS.Clear;
      This.SPI_Transfer (Cmd);
      This.CS.Set;
      Status := To_STATUS_Register (Cmd (1));
   end NOP;

   procedure Initialize
      (This : in out Device)
   is
   begin
      This.Rate_Low := False;
      This.Rate_High := True;
      This.RX_DR := 0;
      This.Mode := Idle;

      This.CE.Clear;
      This.W_REGISTER (DYNPD, 16#00#);       --  Disable dynamic payload length
      This.W_REGISTER (SETUP_RETR, 16#00#);  --  Disable retransmit
      This.W_REGISTER (EN_AA, 16#00#);       --  Disable auto ack on all pipes
      This.W_REGISTER (EN_RXADDR, 16#00#);   --  Disable RX on all pipes
      This.W_REGISTER (RF_CH, 76);           --  Set frequency 2476 MHz
      This.W_REGISTER (CONFIG, To_UInt8 (CONFIG_Register'
         (MASK_RX_DR    => False,
          MASK_TX_DS    => False,
          MASK_MAX_RT   => False,
          EN_CRC        => False,
          CRCO          => False,
          PWR_UP        => True,
          PRIM_RX       => PTX)));
   end Initialize;

   procedure Set_Channel
      (This : in out Device;
       MHz  : NRF_Channel)
   is
   begin
      This.W_REGISTER (RF_CH, UInt8 (Natural (MHz) - Natural (NRF_Channel'First)));
   end Set_Channel;

   procedure Set_Transmit_Address
      (This : in out Device;
       Addr : NRF_Address)
   is
      A : UInt8_Array (1 .. 5) := (others => 0);
   begin
      This.W_REGISTER (SETUP_AW, UInt8 (Addr.Width) - 2);
      A (6 - Addr.Width .. 5) := Addr.Addr;
      This.W_REGISTER (TX_ADDR, A);
   end Set_Transmit_Address;

   procedure Set_Receive_Address
      (This : in out Device;
       Addr : NRF_Address)
   is
      A : UInt8_Array (1 .. 5) := (others => 0);
   begin
      This.W_REGISTER (SETUP_AW, UInt8 (Addr.Width) - 2);
      A (6 - Addr.Width .. 5) := Addr.Addr;
      This.W_REGISTER (RX_ADDR_P1, A);
   end Set_Receive_Address;

   procedure Transmit
      (This  : in out Device;
       Addr  : NRF_Address;
       Data  : UInt8_Array;
       Power : NRF_Transmit_Power := Max_Power)
   is
   begin
      This.CE.Clear;

      This.Set_Transmit_Address (Addr);

      This.W_REGISTER (RF_SETUP, To_UInt8 (RF_SETUP_Register'
         (CONT_WAVE  => False,
          PLL_LOCK   => False,
          RF_DR_LOW  => This.Rate_Low,
          RF_DR_HIGH => This.Rate_High,
          RF_PWR     => UInt2 (NRF_Transmit_Power'Pos (Power)))));
      This.W_REGISTER (CONFIG, To_UInt8 (CONFIG_Register'
         (MASK_RX_DR    => True,
          MASK_TX_DS    => False,
          MASK_MAX_RT   => False,
          EN_CRC        => False,
          CRCO          => False,
          PWR_UP        => True,
          PRIM_RX       => PTX)));

      This.FLUSH_TX;
      This.Clear_Status;
      This.W_TX_PAYLOAD (Data);
      This.Mode := Transmitting;
      This.CE.Set;
   end Transmit;

   procedure Listen
      (This   : in out Device;
       Addr   : NRF_Address;
       Length : NRF_Payload_Length)
   is
   begin
      This.CE.Clear;

      This.W_REGISTER (RF_SETUP, To_UInt8 (RF_SETUP_Register'
         (CONT_WAVE  => False,
          PLL_LOCK   => False,
          RF_DR_LOW  => This.Rate_Low,
          RF_DR_HIGH => This.Rate_High,
          RF_PWR     => 0)));
      This.W_REGISTER (CONFIG, To_UInt8 (CONFIG_Register'
         (MASK_RX_DR    => False,
          MASK_TX_DS    => True,
          MASK_MAX_RT   => True,
          EN_CRC        => False,
          CRCO          => False,
          PWR_UP        => True,
          PRIM_RX       => PRX)));

      This.Set_Receive_Address (Addr);
      This.W_REGISTER (EN_RXADDR, 16#02#);
      This.W_REGISTER (RX_PW_P1, UInt8 (Length));

      This.FLUSH_RX;
      This.Clear_Status;
      This.Mode := Receiving;
      This.RX_DR := 0;
      This.CE.Set;
   end Listen;

   procedure Receive
      (This : in out Device;
       Data : out UInt8_Array)
   is
   begin
      This.R_RX_PAYLOAD (Data);
      This.Clear_Status;
      This.RX_DR := This.RX_DR - 1;
   end Receive;

   procedure Clear_Status
      (This : in out Device)
   is
   begin
      This.W_REGISTER (STATUS, 16#70#);
   end Clear_Status;

   function Data_Ready
      (This : in out Device)
      return Natural
   is
   begin
      return This.RX_DR;
   end Data_Ready;

   procedure Interrupt
      (This : in out Device)
   is
   begin
      case This.Mode is
         when Transmitting =>
            --  TX complete
            This.CE.Clear;
            This.Clear_Status;
            This.Mode := Idle;
         when Receiving =>
            if This.RX_DR < 3 then
               --  There are only 3 FIFO slots, so we've dropped a frame.
               This.RX_DR := This.RX_DR + 1;
            end if;
         when Idle =>
            null;
      end case;
   end Interrupt;

   procedure Set_Data_Rate
      (This : in out Device;
       Rate : NRF_Data_Rate)
   is
   begin
      case Rate is
         when Low_Rate =>
            This.Rate_Low := True;
            This.Rate_High := False;
         when Medium_Rate =>
            This.Rate_Low := False;
            This.Rate_High := False;
         when High_Rate =>
            This.Rate_Low := False;
            This.Rate_High := True;
      end case;
   end Set_Data_Rate;

end NRF24L01;
