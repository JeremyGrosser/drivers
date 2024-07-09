--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Loosely based on https://github.com/kpierzynski/AVR_LoRa
with HAL; use HAL;

generic
   with procedure SPI_Transfer
      (Data : in out HAL.UInt8_Array);
package SX1276
   with Elaborate_Body
is
   procedure Initialize;
   procedure Sleep;
   procedure Standby;
   procedure Listen;
   procedure Interrupt;
   --  Called on the falling edge of DIO_1 (RX_DONE IRQ)

   type Hertz is new Natural;

   subtype Bandwidth is Hertz
      with Static_Predicate => Bandwidth in
         7_800 | 10_400 | 15_600 | 20_800 | 31_250 | 41_700 |
         62_500 | 125_000 | 250_000 | 500_000;

   procedure Set_Bandwidth
      (BW : Bandwidth);

   type Spreading_Factor is range 6 .. 12;

   procedure Set_Spreading_Factor
      (SF : Spreading_Factor);

   type Coding_Rate is range 5 .. 8;

   procedure Set_Coding_Rate
      (CR : Coding_Rate);

   function Last_Packet_RSSI
      return Integer;

   type Decibels is new Integer;
   subtype TX_Power is Decibels range 2 .. 20;

   procedure Set_TX_Power
      (Power : TX_Power);

   procedure Set_Frequency
      (F : Hertz);

   procedure Transmit
      (Data : HAL.UInt8_Array)
   with Pre => Data'Length <= 255;

   function Available
      return Natural;

   procedure Receive
      (Data : out HAL.UInt8_Array)
   with Pre => Data'Length <= Available;

end SX1276;
