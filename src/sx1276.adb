--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body SX1276 is

   REG_FIFO                   : constant := 16#00#;
   REG_OP_MODE                : constant := 16#01#;
   REG_FRF_MSB                : constant := 16#06#;
   REG_FRF_MID                : constant := 16#07#;
   REG_FRF_LSB                : constant := 16#08#;
   REG_PA_CONFIG              : constant := 16#09#;
   REG_OCP                    : constant := 16#0B#;
   REG_LNA                    : constant := 16#0C#;
   REG_FIFO_ADDR_PTR          : constant := 16#0D#;
   REG_FIFO_TX_BASE_ADDR      : constant := 16#0E#;
   REG_FIFO_RX_BASE_ADDR      : constant := 16#0F#;
   REG_FIFO_RX_CURRENT_ADDR   : constant := 16#10#;
   REG_IRQ_FLAGS              : constant := 16#12#;
   REG_RX_NB_BYTES            : constant := 16#13#;
   REG_PKT_RSSI_VALUE         : constant := 16#1A#;
   REG_MODEM_CONFIG_1         : constant := 16#1D#;
   REG_MODEM_CONFIG_2         : constant := 16#1E#;
   REG_PAYLOAD_LENGTH         : constant := 16#22#;
   REG_MODEM_CONFIG_3         : constant := 16#26#;
   REG_DIO_MAPPING_1          : constant := 16#40#;
   REG_VERSION                : constant := 16#42#;
   REG_PA_DAC                 : constant := 16#4D#;

   MODE_SLEEP           : constant := 16#00#;
   MODE_STDBY           : constant := 16#01#;
   MODE_TX              : constant := 16#03#;
   MODE_RX_CONTINUOUS   : constant := 16#05#;
   MODE_LONG_RANGE_MODE : constant := 16#80#;

   PA_BOOST             : constant := 16#80#;

   IRQ_TX_DONE_MASK  : constant := 16#08#;
   IRQ_RX_DONE_MASK  : constant := 16#40#;

   RF_MID_BAND_THRESHOLD   : constant := 525_000_000;
   RSSI_OFFSET_HF_PORT     : constant := 157;
   RSSI_OFFSET_LF_PORT     : constant := 164;

   function Read_Reg
      (This : in out Device;
       Reg  : UInt7)
       return UInt8
   is
      use HAL.SPI;
      Data   : SPI_Data_8b (1 .. 2) := (UInt8 (Reg), 16#00#);
      Status : SPI_Status;
   begin
      --  Assumes This.Port can buffer at least 2 bytes for receive
      This.Port.Transmit (Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.Port.Receive (Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      return Data (2);
   end Read_Reg;

   procedure Write_Reg
      (This : in out Device;
       Reg  : UInt7;
       Val  : UInt8)
   is
      use HAL.SPI;
      Data   : SPI_Data_8b (1 .. 2) := (UInt8 (Reg) or 16#80#, Val);
      Status : SPI_Status;
   begin
      This.Port.Transmit (Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.Port.Receive (Data, Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
   end Write_Reg;

   procedure Initialize
      (This : in out Device)
   is
      Version : UInt8;
   begin
      Byte_Buffers.Clear (This.RX_Buffer);

      Version := This.Read_Reg (REG_VERSION);
      if Version /= 16#12# then
         raise Program_Error with "Unknown SX1276 version: " & Version'Image;
      end if;

      This.Sleep;
      This.Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE);
      This.Set_Frequency (915_000_000);

      This.Write_Reg (REG_FIFO_TX_BASE_ADDR, 0);
      This.Write_Reg (REG_FIFO_RX_BASE_ADDR, 0);

      --  LNA Boost HF
      This.Write_Reg (REG_LNA, This.Read_Reg (REG_LNA) or 2#11#);

      --  LowDataRateOptimize | AgcAutoOn
      This.Write_Reg (REG_MODEM_CONFIG_3, 2#100#);

      --  DIO0 is RX_DONE IRQ
      This.Write_Reg (REG_DIO_MAPPING_1, 16#00#);

      This.Set_Bandwidth (125_000);
      This.Set_Spreading_Factor (7);
      This.Set_Coding_Rate (5);

      This.Set_TX_Power (20);

      --  Explicit header mode
      This.Write_Reg (REG_MODEM_CONFIG_1,
         This.Read_Reg (REG_MODEM_CONFIG_1) and 2#1111_1110#);

      This.Standby;
      This.Delays.Delay_Milliseconds (50);
      This.Listen;
   end Initialize;

   procedure Sleep
      (This : in out Device)
   is
   begin
      This.Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_SLEEP);
   end Sleep;

   procedure Standby
      (This : in out Device)
   is
   begin
      This.Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_STDBY);
   end Standby;

   procedure Listen
      (This : in out Device)
   is
   begin
      This.Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_RX_CONTINUOUS);
   end Listen;

   procedure Interrupt
      (This : in out Device)
   is
      Len : UInt8;
      IRQ : UInt8;
   begin
      IRQ := This.Read_Reg (REG_IRQ_FLAGS);

      --  Clear interrupts
      This.Write_Reg (REG_IRQ_FLAGS, IRQ);

      if (IRQ and IRQ_RX_DONE_MASK) /= 0 then
         Len := This.Read_Reg (REG_RX_NB_BYTES);
         This.Write_Reg (REG_FIFO_ADDR_PTR,
            This.Read_Reg (REG_FIFO_RX_CURRENT_ADDR));
         for I in 1 .. Len loop
            Byte_Buffers.Append (This.RX_Buffer, This.Read_Reg (REG_FIFO));
         end loop;
      end if;
   end Interrupt;

   procedure Set_Bandwidth
      (This : in out Device;
       BW   : Bandwidth)
   is
      Mode : UInt8;
   begin
      case BW is
         when 7_800     => Mode := 0;
         when 10_400    => Mode := 1;
         when 15_600    => Mode := 2;
         when 20_800    => Mode := 3;
         when 31_250    => Mode := 4;
         when 41_700    => Mode := 5;
         when 62_500    => Mode := 6;
         when 125_000   => Mode := 7;
         when 250_000   => Mode := 8;
         when 500_000   => Mode := 9;
      end case;

      This.Write_Reg (REG_MODEM_CONFIG_1,
         (This.Read_Reg (REG_MODEM_CONFIG_1) and 2#0000_1111#)
         or Shift_Left (Mode, 4));
   end Set_Bandwidth;

   procedure Set_Spreading_Factor
      (This : in out Device;
       SF   : Spreading_Factor)
   is
   begin
      This.Write_Reg (REG_MODEM_CONFIG_2,
         (This.Read_Reg (REG_MODEM_CONFIG_2) and 2#0000_1111#)
         or Shift_Left (UInt8 (SF), 4));
   end Set_Spreading_Factor;

   procedure Set_Coding_Rate
      (This : in out Device;
       CR   : Coding_Rate)
   is
   begin
      This.Write_Reg (REG_MODEM_CONFIG_1,
         (This.Read_Reg (REG_MODEM_CONFIG_1) and 2#1111_0001#)
         or Shift_Left (UInt8 (CR), 1));
   end Set_Coding_Rate;

   function Last_Packet_RSSI
      (This : in out Device)
       return Integer
   is
      RSSI : Integer;
   begin
      RSSI := Integer (This.Read_Reg (REG_PKT_RSSI_VALUE));
      if This.Freq < RF_MID_BAND_THRESHOLD then
         RSSI := RSSI + RSSI_OFFSET_LF_PORT;
      else
         RSSI := RSSI + RSSI_OFFSET_HF_PORT;
      end if;
      return (-RSSI);
   end Last_Packet_RSSI;

   procedure Set_Max_Current
      (This : in out Device;
       I    : Milliamps)
   is
      Trim : UInt8;
   begin
      if I > 240 then
         return;
      elsif I <= 120 then
         Trim := (I - 45) / 5;
      elsif I <= 240 then
         Trim := (I + 30) / 10;
      else
         Trim := 27;
      end if;

      This.Write_Reg (REG_OCP, Shift_Left (1, 5) or (Trim and 2#0001_1111#));
   end Set_Max_Current;

   procedure Set_TX_Power
      (This  : in out Device;
       Power : TX_Power)
   is
   begin
      This.Set_Max_Current (150);
      if Power > 17 then
         This.Write_Reg (REG_PA_DAC, 16#87#);
         This.Write_Reg (REG_PA_CONFIG, PA_BOOST or (UInt8 (Power - 2) - 3));
      else
         This.Write_Reg (REG_PA_DAC, 16#84#);
         This.Write_Reg (REG_PA_CONFIG, PA_BOOST or (UInt8 (Power - 2) - 2));
      end if;
   end Set_TX_Power;

   procedure Set_Frequency
      (This : in out Device;
       Freq : Hertz)
   is
      F_XOSC : constant := 32_000_000;
      FRF    : constant UInt64 := Shift_Left (UInt64 (Freq), 19) / F_XOSC;
   begin
      This.Write_Reg (REG_FRF_MSB, UInt8 (Shift_Right (FRF, 16) and 16#FF#));
      This.Write_Reg (REG_FRF_MID, UInt8 (Shift_Right (FRF, 8) and 16#FF#));
      This.Write_Reg (REG_FRF_LSB, UInt8 (Shift_Right (FRF, 0) and 16#FF#));
      This.Freq := Freq;
   end Set_Frequency;

   procedure Transmit
      (This : in out Device;
       Data : HAL.UInt8_Array)
   is
   begin
      This.Standby;
      This.Write_Reg (REG_FIFO_ADDR_PTR, 0);
      for D of Data loop
         This.Write_Reg (REG_FIFO, D);
      end loop;
      This.Write_Reg (REG_PAYLOAD_LENGTH, UInt8 (Data'Length));
      This.Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_TX);
      loop
         exit when (This.Read_Reg (REG_IRQ_FLAGS) and IRQ_TX_DONE_MASK) /= 0;
      end loop;
      This.Write_Reg (REG_IRQ_FLAGS, IRQ_TX_DONE_MASK);
      This.Listen;
   end Transmit;

   procedure Receive
      (This : in out Device;
       Data : out HAL.UInt8_Array)
   is
      use Byte_Buffers;
   begin
      for I in Data'Range loop
         Data (I) := First_Element (This.RX_Buffer);
         Delete_First (This.RX_Buffer);
      end loop;
   end Receive;

   function Available
      (This : Device)
      return Natural
   is (Byte_Buffers.Length (This.RX_Buffer));

end SX1276;
