--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Loosely based on https://github.com/kpierzynski/AVR_LoRa
private with Chests.Ring_Buffers;
with HAL; use HAL;
with HAL.Time;
with HAL.SPI;

package SX1276
   with Preelaborate
is

   type Device
      (Port   : not null HAL.SPI.Any_SPI_Port;
       Delays : not null HAL.Time.Any_Delays)
   is tagged private;

   procedure Initialize
      (This : in out Device);

   procedure Sleep
      (This : in out Device);

   procedure Standby
      (This : in out Device);

   procedure Listen
      (This : in out Device);

   procedure Interrupt
      (This : in out Device);
   --  Called on the falling edge of DIO_1 (RX_DONE IRQ)

   type Hertz is new Natural;

   subtype Bandwidth is Hertz
      with Static_Predicate => Bandwidth in
         7_800 | 10_400 | 15_600 | 20_800 | 31_250 | 41_700 |
         62_500 | 125_000 | 250_000 | 500_000;

   procedure Set_Bandwidth
      (This : in out Device;
       BW   : Bandwidth);

   type Spreading_Factor is range 6 .. 12;

   procedure Set_Spreading_Factor
      (This : in out Device;
       SF   : Spreading_Factor);

   type Coding_Rate is range 5 .. 8;

   procedure Set_Coding_Rate
      (This : in out Device;
       CR   : Coding_Rate);

   function Last_Packet_RSSI
      (This : in out Device)
       return Integer;

   type Decibels is new Integer;
   subtype TX_Power is Decibels range 2 .. 20;

   procedure Set_TX_Power
      (This  : in out Device;
       Power : TX_Power);

   procedure Set_Frequency
      (This : in out Device;
       Freq : Hertz);

   procedure Transmit
      (This : in out Device;
       Data : HAL.UInt8_Array)
   with Pre => Data'Length <= 255;

   function Available
      (This : Device)
      return Natural;

   procedure Receive
      (This : in out Device;
       Data : out HAL.UInt8_Array)
   with Pre => Data'Length <= This.Available;

private

   package Byte_Buffers is new Chests.Ring_Buffers
      (Element_Type => HAL.UInt8,
       Capacity => 255 * 2);
   --  The SX1276 FIFO is 255 bytes deep, we keep enough space here to buffer
   --  two transfers

   type Device
      (Port   : not null HAL.SPI.Any_SPI_Port;
       Delays : not null HAL.Time.Any_Delays)
   is tagged record
      RX_Buffer : Byte_Buffers.Ring_Buffer;
      Freq      : Hertz;
   end record;

   function Read_Reg
      (This : in out Device;
       Reg  : UInt7)
       return UInt8;

   procedure Write_Reg
      (This : in out Device;
       Reg  : UInt7;
       Val  : UInt8);

   subtype Milliamps is UInt8;

   procedure Set_Max_Current
      (This : in out Device;
       I    : Milliamps);

end SX1276;
