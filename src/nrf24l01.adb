with NRF24L01_Registers; use NRF24L01_Registers;

package body NRF24L01 is

   procedure Initialize
      (This   : in out Device;
       P      : Pins;
       Delays : not null HAL.Time.Any_Delays)
   is
   begin
      This.P := P;
      This.Delays := Delays;
      This.Reset;

      CONFIG.Write (This.P, CONFIG_Register'
         (EN_CRC  => True,
          CRCO    => True,
          PRIM_RX => PRX,
          PWR_UP  => False,
          others  => <>),
      Verify => True);
      --  Verify the first write just to be sure the SPI interface is working.

      SETUP_RETR.Write (This.P, SETUP_RETR_Register'
         (ARD => 15,   --  Auto Retransmit Delay (1_500 microseconds between retransmits)
          ARC => 15)); --  Auto Retransmit Count (up to 15 times)
      RF_SETUP.Write (This.P, RF_SETUP_Register'
         (CONT_WAVE  => False,
          RF_DR_LOW  => False,
          PLL_LOCK   => False,
          RF_DR_HIGH => False,   --  1 Mbps
          RF_PWR     => 2#11#)); --  Max transmit power
      --  Disable dynamic payloads
      DYNPD.Write (This.P, DYNPD_Register'(others => False));
      --  Enable auto-ack on all pipes
      EN_AA.Write (This.P, EN_AA_Register'(others => True));
      --  Open RX pipes 0 and 1
      EN_RXADDR.Write (This.P, EN_RXADDR_Register'
         (P0     => True,
          P1     => True,
          others => False));
      --  Set static payload size to max (32 bytes) for all pipes
      Set_Payload_Length (This, NRF_Data_Length'Last);
      --  5 byte addressing
      This.AW := 5;
      SETUP_AW.Write (This.P, AW_5_Bytes);
      --  Default to channel 76, which won't bleed into other bands.
      RF_CH.Write (This.P, 76);
      --  Clear status flags
      STATUS.Write (This.P, STATUS_Register'
         (RX_DR  => True,
          TX_DS  => True,
          MAX_RT => True,
          others => <>));

      NRF24L01_IO.Flush_RX (This.P);
      NRF24L01_IO.Flush_TX (This.P);

      declare
         C : CONFIG_Register := CONFIG.Read (This.P);
      begin
         C.PWR_UP := True;
         CONFIG.Write (This.P, C);
         This.Delays.Delay_Milliseconds (5); --  Tpd2stdby
         if CONFIG.Read (This.P) /= C then
            raise NRF_Error with "Power up failed";
         end if;
      end;
   end Initialize;

   procedure Reset
      (This : in out Device)
   is
      use HAL.Time;
      Tpor : constant := 100;
   begin
      This.P.CS.Set;
      This.P.CE.Clear;
      This.Delays.Delay_Milliseconds (Tpor);
   end Reset;

   procedure Set_Output_Power
      (This : in out Device;
       dBm  : Power_dBm)
   is
      Reg : RF_SETUP_Register := RF_SETUP.Read (This.P);
   begin
      case dBm is
         when -18 => Reg.RF_PWR := 2#00#;
         when -12 => Reg.RF_PWR := 2#01#;
         when -6  => Reg.RF_PWR := 2#10#;
         when 0   => Reg.RF_PWR := 2#11#;
         when others =>
            raise NRF_Error with "Invalid power amplifier setting";
      end case;
      RF_SETUP.Write (This.P, Reg);
   end Set_Output_Power;

   procedure Set_Payload_Length
      (This  : in out Device;
       Bytes : NRF_Data_Length)
   is
   begin
      This.Length := Bytes;
      RX_PW_P0.Write (This.P, RX_PW_Register (Bytes));
      RX_PW_P1.Write (This.P, RX_PW_Register (Bytes));
      RX_PW_P2.Write (This.P, RX_PW_Register (Bytes));
      RX_PW_P3.Write (This.P, RX_PW_Register (Bytes));
      RX_PW_P4.Write (This.P, RX_PW_Register (Bytes));
      RX_PW_P5.Write (This.P, RX_PW_Register (Bytes));
   end Set_Payload_Length;

   function Payload_Length
      (This : Device)
      return NRF_Data_Length
   is (This.Length);

   procedure Configure_Receive
      (This : in out Device;
       Pipe : Data_Pipe;
       Addr : NRF_Address)
   is
      EN : EN_RXADDR_Register;
      AW : SETUP_AW_Register;
      RA : UInt40;
   begin
      case Addr.Width is
         when 3 =>
            RA := UInt40 (Addr.Addr_3);
            AW := AW_3_Bytes;
         when 4 =>
            RA := UInt40 (Addr.Addr_4);
            AW := AW_4_Bytes;
         when 5 =>
            RA := Addr.Addr_5;
            AW := AW_5_Bytes;
      end case;

      if Addr.Width /= This.AW then
         SETUP_AW.Write (This.P, AW);
         This.AW := Addr.Width;
      end if;

      case Pipe is
         when 0 => RX_ADDR_P0.Write (This.P, RX_ADDR_P0_Register (RA));
         when 1 => RX_ADDR_P1.Write (This.P, RX_ADDR_P1_Register (RA));
         when 2 => RX_ADDR_P2.Write (This.P, RX_ADDR_P2_Register (RA and 16#FF#));
         when 3 => RX_ADDR_P3.Write (This.P, RX_ADDR_P3_Register (RA and 16#FF#));
         when 4 => RX_ADDR_P4.Write (This.P, RX_ADDR_P4_Register (RA and 16#FF#));
         when 5 => RX_ADDR_P5.Write (This.P, RX_ADDR_P5_Register (RA and 16#FF#));
      end case;

      EN := EN_RXADDR.Read (This.P);
      EN (Data_Pipe_Index'Val (Natural (Pipe))) := True;
      EN_RXADDR.Write (This.P, EN);
   end Configure_Receive;

   procedure Configure_Transmit
      (This : in out Device;
       Addr : NRF_Address)
   is
      TA : TX_ADDR_Register;
      AW : SETUP_AW_Register;
   begin
      --  Receive pipe 0 needs to have the same address as the transmitter in
      --  order to handle auto-ack.
      Configure_Receive (This, 0, Addr);

      case Addr.Width is
         when 3 =>
            TA := TX_ADDR_Register (Addr.Addr_3);
            AW := AW_3_Bytes;
         when 4 =>
            TA := TX_ADDR_Register (Addr.Addr_4);
            AW := AW_4_Bytes;
         when 5 =>
            TA := TX_ADDR_Register (Addr.Addr_5);
            AW := AW_5_Bytes;
      end case;

      if Addr.Width /= This.AW then
         SETUP_AW.Write (This.P, AW);
         This.AW := Addr.Width;
      end if;

      TX_ADDR.Write (This.P, TA);
   end Configure_Transmit;

   procedure Receive
      (This : in out Device;
       Pipe : Data_Pipe;
       Addr : out NRF_Address;
       Data : out UInt8_Array)
   is
      pragma Unreferenced (Pipe);
      pragma Unreferenced (Addr);
      pragma Unreferenced (Data);
      C : CONFIG_Register := CONFIG.Read (This.P);
   begin
      C.PRIM_RX := PRX;
      C.PWR_UP := True;
      CONFIG.Write (This.P, C);
      This.Delays.Delay_Milliseconds (5); --  Tpd2stdby

      --  Clear STATUS
      STATUS.Write (This.P, STATUS_Register'
         (RX_DR  => True,
          TX_DS  => True,
          MAX_RT => True,
          others => <>));

      This.P.CE.Set;
      --  This.Delays.Delay_Milliseconds (1); --  Thce >= 10us
   end Receive;

   function Available
      (This : in out Device;
       Pipe : Data_Pipe)
       return Boolean
   is (NRF24L01_IO.Get_Status (This.P).RX_P_NO = UInt3 (Pipe));

end NRF24L01;
