--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Chests.Ring_Buffers;

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

   package Byte_Buffers is new Chests.Ring_Buffers
      (Element_Type => HAL.UInt8,
       Capacity     => 255 * 2);
   RX_Buffer : Byte_Buffers.Ring_Buffer;

   Freq : Hertz;

   function Read_Reg
      (Reg : UInt7)
      return UInt8
   is
      Data : UInt8_Array (1 .. 2) := (UInt8 (Reg), 16#00#);
   begin
      SPI_Transfer (Data);
      return Data (2);
   end Read_Reg;

   procedure Write_Reg
      (Reg  : UInt7;
       Val  : UInt8)
   is
      Data : UInt8_Array (1 .. 2) := (UInt8 (Reg) or 16#80#, Val);
   begin
      SPI_Transfer (Data);
   end Write_Reg;

   procedure Initialize is
      Version : UInt8;
   begin
      Byte_Buffers.Clear (RX_Buffer);

      Version := Read_Reg (REG_VERSION);
      if Version /= 16#12# then
         raise Program_Error with "Unknown SX1276 version: " & Version'Image;
      end if;

      Sleep;
      Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE);
      Set_Frequency (915_000_000);

      Write_Reg (REG_FIFO_TX_BASE_ADDR, 0);
      Write_Reg (REG_FIFO_RX_BASE_ADDR, 0);

      --  LNA Boost HF
      Write_Reg (REG_LNA, Read_Reg (REG_LNA) or 2#11#);

      --  LowDataRateOptimize | AgcAutoOn
      Write_Reg (REG_MODEM_CONFIG_3, 2#100#);

      --  DIO0 is RX_DONE IRQ
      Write_Reg (REG_DIO_MAPPING_1, 16#00#);

      Set_Bandwidth (125_000);
      Set_Spreading_Factor (7);
      Set_Coding_Rate (5);

      Set_TX_Power (20);

      --  Explicit header mode
      Write_Reg (REG_MODEM_CONFIG_1, Read_Reg (REG_MODEM_CONFIG_1) and 2#1111_1110#);

      Standby;
   end Initialize;

   procedure Sleep is
   begin
      Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_SLEEP);
   end Sleep;

   procedure Standby is
   begin
      Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_STDBY);
   end Standby;

   procedure Listen is
   begin
      Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_RX_CONTINUOUS);
   end Listen;

   procedure Interrupt is
      Len : UInt8;
      IRQ : UInt8;
   begin
      IRQ := Read_Reg (REG_IRQ_FLAGS);

      --  Clear interrupts
      Write_Reg (REG_IRQ_FLAGS, IRQ);

      if (IRQ and IRQ_RX_DONE_MASK) /= 0 then
         Len := Read_Reg (REG_RX_NB_BYTES);
         Write_Reg (REG_FIFO_ADDR_PTR, Read_Reg (REG_FIFO_RX_CURRENT_ADDR));
         for I in 1 .. Len loop
            Byte_Buffers.Append (RX_Buffer, Read_Reg (REG_FIFO));
         end loop;
      end if;
   end Interrupt;

   procedure Set_Bandwidth
      (BW : Bandwidth)
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

      Write_Reg (REG_MODEM_CONFIG_1,
         (Read_Reg (REG_MODEM_CONFIG_1) and 2#0000_1111#)
         or Shift_Left (Mode, 4));
   end Set_Bandwidth;

   procedure Set_Spreading_Factor
      (SF : Spreading_Factor)
   is
   begin
      Write_Reg (REG_MODEM_CONFIG_2,
         (Read_Reg (REG_MODEM_CONFIG_2) and 2#0000_1111#)
         or Shift_Left (UInt8 (SF), 4));
   end Set_Spreading_Factor;

   procedure Set_Coding_Rate
      (CR : Coding_Rate)
   is
   begin
      Write_Reg (REG_MODEM_CONFIG_1,
         (Read_Reg (REG_MODEM_CONFIG_1) and 2#1111_0001#)
         or Shift_Left (UInt8 (CR), 1));
   end Set_Coding_Rate;

   function Last_Packet_RSSI
       return Integer
   is
      RSSI : Integer;
   begin
      RSSI := Integer (Read_Reg (REG_PKT_RSSI_VALUE));
      if Freq < RF_MID_BAND_THRESHOLD then
         RSSI := RSSI + RSSI_OFFSET_LF_PORT;
      else
         RSSI := RSSI + RSSI_OFFSET_HF_PORT;
      end if;
      return (-RSSI);
   end Last_Packet_RSSI;

   subtype Milliamps is UInt8;

   procedure Set_Max_Current
      (I : Milliamps)
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

      Write_Reg (REG_OCP, Shift_Left (1, 5) or (Trim and 2#0001_1111#));
   end Set_Max_Current;

   procedure Set_TX_Power
      (Power : TX_Power)
   is
   begin
      Set_Max_Current (150);
      if Power > 17 then
         Write_Reg (REG_PA_DAC, 16#87#);
         Write_Reg (REG_PA_CONFIG, PA_BOOST or (UInt8 (Power - 2) - 3));
      else
         Write_Reg (REG_PA_DAC, 16#84#);
         Write_Reg (REG_PA_CONFIG, PA_BOOST or (UInt8 (Power - 2) - 2));
      end if;
   end Set_TX_Power;

   procedure Set_Frequency
      (F : Hertz)
   is
      F_XOSC : constant := 32_000_000;
      FRF    : constant UInt64 := Shift_Left (UInt64 (F), 19) / F_XOSC;
   begin
      Write_Reg (REG_FRF_MSB, UInt8 (Shift_Right (FRF, 16) and 16#FF#));
      Write_Reg (REG_FRF_MID, UInt8 (Shift_Right (FRF, 8) and 16#FF#));
      Write_Reg (REG_FRF_LSB, UInt8 (Shift_Right (FRF, 0) and 16#FF#));
      Freq := F;
   end Set_Frequency;

   procedure Transmit
      (Data : HAL.UInt8_Array)
   is
   begin
      Standby;
      Write_Reg (REG_FIFO_ADDR_PTR, 0);
      for D of Data loop
         Write_Reg (REG_FIFO, D);
      end loop;
      Write_Reg (REG_PAYLOAD_LENGTH, UInt8 (Data'Length));
      Write_Reg (REG_OP_MODE, MODE_LONG_RANGE_MODE or MODE_TX);
      loop
         exit when (Read_Reg (REG_IRQ_FLAGS) and IRQ_TX_DONE_MASK) /= 0;
      end loop;
      Write_Reg (REG_IRQ_FLAGS, IRQ_TX_DONE_MASK);
      Listen;
   end Transmit;

   procedure Receive
      (Data : out HAL.UInt8_Array)
   is
      use Byte_Buffers;
   begin
      for I in Data'Range loop
         Data (I) := First_Element (RX_Buffer);
         Delete_First (RX_Buffer);
      end loop;
   end Receive;

   function Available
      return Natural
   is (Byte_Buffers.Length (RX_Buffer));

end SX1276;
