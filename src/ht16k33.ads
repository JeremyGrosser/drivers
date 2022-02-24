--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package HT16K33 is
   type Device
      (Port    : not null Any_I2C_Port;
       Address : I2C_Address)
   is tagged limited record
      --  byte 0 is the address command
      Buffer : UInt8_Array (0 .. 16) := (0 => 0, others => 16#FF#);
   end record;

   subtype Brightness_Level is UInt8 range 0 .. 15;
   subtype Output_Index is Natural range 0 .. 127;

   procedure Initialize
      (This : in out Device);

   procedure Set_Brightness
      (This  : in out Device;
       Level : Brightness_Level);

   procedure Set
      (This : in out Device;
       Num  : Output_Index);

   procedure Clear
      (This : in out Device;
       Num  : Output_Index);

   procedure Clear_All
      (This : in out Device);

   procedure Update
      (This : in out Device);

end HT16K33;
