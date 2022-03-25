--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with ADXL345_SPI;
with HAL;

package ADXL345 is

   package DEVID is new ADXL345_SPI.Register (16#00#, HAL.UInt8);

   type POWER_CTL_Wakeup_Field is
      (Wake_8_Hz, Wake_4_Hz, Wake_2_Hz, Wake_1_Hz)
      with Size => 2;

   type POWER_CTL_Register is record
      Link        : Boolean;
      AUTO_SLEEP  : Boolean;
      Measure     : Boolean;
      Sleep       : Boolean;
      Wakeup      : POWER_CTL_Wakeup_Field;
   end record
      with Size => 8;

   for POWER_CTL_Register use record
      Link        at 0 range 5 .. 5;
      AUTO_SLEEP  at 0 range 4 .. 4;
      Measure     at 0 range 3 .. 3;
      Sleep       at 0 range 2 .. 2;
      Wakeup      at 0 range 0 .. 1;
   end record;

   package POWER_CTL is new ADXL345_SPI.Register (16#2D#, POWER_CTL_Register);

   type DATA_FORMAT_G_Range_Field is (Range_2g, Range_4g, Range_8g, Range_16g)
      with Size => 2;

   type DATA_FORMAT_Register is record
      SELF_TEST   : Boolean;
      SPI         : Boolean;
      INT_INVERT  : Boolean;
      FULL_RES    : Boolean;
      Justify     : Boolean;
      G_Range     : DATA_FORMAT_G_Range_Field;
   end record
      with Size => 8;
   for DATA_FORMAT_Register use record
      SELF_TEST   at 0 range 7 .. 7;
      SPI         at 0 range 6 .. 6;
      INT_INVERT  at 0 range 5 .. 5;
      FULL_RES    at 0 range 3 .. 3;
      Justify     at 0 range 2 .. 2;
      G_Range     at 0 range 0 .. 1;
   end record;

   package DATA_FORMAT is new ADXL345_SPI.Register (16#31#, DATA_FORMAT_Register);

   type Axis is (X, Y, Z);
   type Measurement is array (Axis) of HAL.UInt16;
   package DATA is new ADXL345_SPI.Register (16#32#, Measurement);

   subtype Acceleration is Float;
   function To_Acceleration
      (G_Range : DATA_FORMAT_G_Range_Field;
       A       : HAL.UInt16)
      return Acceleration;

end ADXL345;
