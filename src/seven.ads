--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;
with HT16K33;

package Seven is
   type Device is new HT16K33.Device with private;

   subtype Position is Integer range 1 .. 4;
   subtype Digit is Integer range 0 .. 9;
   subtype Point_Number is Integer range 1 .. 4;

   procedure Set_Digit
      (This : in out Device;
       Pos  : Position;
       Val  : Digit);

   procedure Set_Colon
      (This : in out Device;
       On   : Boolean);

   procedure Set_Point
      (This : in out Device;
       Num  : Point_Number;
       On   : Boolean);

private

   Numbers : constant array (Digit) of UInt8 :=
      (0 => 16#3F#,
       1 => 16#06#,
       2 => 16#5B#,
       3 => 16#4F#,
       4 => 16#66#,
       5 => 16#6D#,
       6 => 16#7D#,
       7 => 16#07#,
       8 => 16#7F#,
       9 => 16#6F#);

   Points : constant array (Point_Number) of HT16K33.Output_Index :=
      (7, 23, 55, 71);

   type Device is new HT16K33.Device with null record;

end Seven;
