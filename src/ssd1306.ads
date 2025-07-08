--
--  Copyright (C) 2024 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: MIT
--
with HAL; use HAL;

generic
   with procedure Write (Data : UInt8_Array);
package SSD1306
   with Preelaborate
is
   Width    : constant := 128;
   Height   : constant := 32;

   subtype Column is Integer range 0 .. Width - 1;
   subtype Row    is Integer range 0 .. Height - 1;

   procedure Initialize;

   procedure Set_Pixel
      (X   : Column;
       Y   : Row;
       Set : Boolean);

   procedure Swap;

   procedure Update;

   procedure Clear;

end SSD1306;
