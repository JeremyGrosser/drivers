--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--

--
--  Oddly Specific Objects
--  LCD FeatherWing
--
--  https://oddlyspecificobjects.com/products/lcdwing/
--  https://www.tindie.com/products/joeycastillo/lcd-featherwing/
--  https://github.com/joeycastillo/LCD-FeatherWing
--
--  This package is loosely based on:
--  https://github.com/joeycastillo/OSO_Arduino_LCD
--
with Ada.Unchecked_Conversion;
with HAL; use HAL;
with BU9796;

package OSO is

   type Indicators is record
      AM, PM, Battery, Colon, Bell, Wifi, Data, Moon : Boolean := False;
   end record;

   type Position is range 1 .. 5;

   Bus_Error : exception;

   type LCD_Wing is new BU9796.Device with record
      Cursor : Position := Position'First;
   end record;

   procedure Update
      (This : in out LCD_Wing);

   --  Update writes the Buffer to the LCD. The following procedures only
   --  modify the Buffer. It's up to you to call Update afterwards.

   procedure Clear
      (This : in out LCD_Wing);
   --  Turn off all segments and reset Cursor to Position 1

   procedure Fill
      (This : in out LCD_Wing);
   --  Turn on all segments

   procedure Set_Indicators
      (This  : in out LCD_Wing;
       Value : Indicators);

   procedure Set_Decimal
      (This : in out LCD_Wing;
       Pos  : Position;
       On   : Boolean);
   --  Sets the decimal point segment. At Pos = 1, this sets the leading dash
   --  instead

   procedure Set
      (This : in out LCD_Wing;
       Pos  : Position;
       Ch   : Character);
   --  Sets an alphanumeric character

   procedure Put
      (This : in out LCD_Wing;
       Ch   : Character);
   --  Sets the character at the Position indicated by Cursor, then increments
   --  the Cursor

   procedure Put
      (This : in out LCD_Wing;
       S    : String);
   --  Fill all 5 positions with characters.
   --  If the first character is '-', the leading dash segment is shown.
   --  If a ':' is at (S'First + 2), the Colon indicator is turned on.
   --  The '.' character enables the decimal point segment at the cursor position.
   --  If S contains more than 5 alphanumeric characters, the last character will be overwritten.
   --  Unrepresentable characters are displayed with the top and bottom segments. (H and A in the diagram below)

private

   for Indicators'Size use 8;
   for Indicators use record
      AM       at 0 range 7 .. 7;
      PM       at 0 range 6 .. 6;
      Battery  at 0 range 5 .. 5;
      Colon    at 0 range 4 .. 4;
      Bell     at 0 range 3 .. 3;
      Wifi     at 0 range 2 .. 2;
      Data     at 0 range 1 .. 1;
      Moon     at 0 range 0 .. 0;
   end record;
   function To_UInt8 is new Ada.Unchecked_Conversion (Indicators, UInt8);

   --     H
   --    ---
   --  G|   |D
   --    ---   <-- B
   --  E|   |C
   --    ---
   --     A   .F
   --
   --  8 Bits: HGFE_DCBA
   --  In position 1, bit F is the leading '-' rather than a '.'

   --  Character set originally from
   --  https://github.com/joeycastillo/OSO_Arduino_LCD
   --
   --  Copyright (c) 2012 Joey Castillo for Oddly Specific Objects
   --  SPDX-License-Identifier: MIT
   --
   --  Modified '7' and '9' to remove unwanted segments.
   --  Added others value displaying top and bottom segments for missing characters

   type Character_Lookup is array (Character) of UInt8
      with Pack;
   Lookup : constant Character_Lookup := (
      ' ' => 2#0000_0000#,
      '!' => 2#0010_1100#,
      '"' => 2#0100_1000#,
      '#' => 2#1100_1010#,
      '$' => 2#0101_0101#,
      '%' => 2#0100_0100#,
      '&' => 2#1101_1000#,
      ''' => 2#0000_1000#,
      '(' => 2#1101_0001#,
      ')' => 2#1000_1101#,
      '+' => 2#0101_0010#,
      ',' => 2#0000_0100#,
      '-' => 2#0000_0010#,
      '.' => 2#0010_0000#,
      '/' => 2#0000_1100#,
      '0' => 2#1101_1101#,
      '1' => 2#0000_1100#,
      '2' => 2#1001_1011#,
      '3' => 2#1000_1111#,
      '4' => 2#0100_1110#,
      '5' => 2#1100_0111#,
      '6' => 2#1101_0111#,
      '7' => 2#1000_1100#,
      '8' => 2#1101_1111#,
      '9' => 2#1000_1110#,
      '<' => 2#0001_0001#,
      '=' => 2#0000_0011#,
      '>' => 2#0000_0101#,
      '?' => 2#1001_1010#,
      '@' => 2#1111_1111#,
      'A' => 2#1101_1110#,
      'B' => 2#0101_0111#,
      'C' => 2#0001_0011#,
      'D' => 2#0001_1111#,
      'E' => 2#1101_0011#,
      'F' => 2#1101_0010#,
      'G' => 2#1100_1111#,
      'H' => 2#0101_0110#,
      'I' => 2#0001_0000#,
      'J' => 2#0001_1101#,
      'K' => 2#1101_0110#,
      'L' => 2#0101_0001#,
      'M' => 2#1101_1100#,
      'N' => 2#0001_0110#,
      'O' => 2#0001_0111#,
      'P' => 2#1101_1010#,
      'Q' => 2#1100_1110#,
      'R' => 2#0001_0010#,
      'S' => 2#1100_0101#,
      'T' => 2#0101_0011#,
      'U' => 2#0001_0101#,
      'V' => 2#0101_1101#,
      'W' => 2#0101_1111#,
      'X' => 2#0101_1110#,
      'Y' => 2#0100_1111#,
      'Z' => 2#1001_1001#,
      others => 2#1000_0001#);

end OSO;
