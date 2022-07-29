--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.I2C; use HAL.I2C;

package body OSO is

   procedure Set_Decimal
      (This : in out LCD_Wing;
       Pos  : Position;
       On   : Boolean)
   is
      I   : constant BU9796.Buffer_Index := BU9796.Buffer_Index (Pos) + 2;
      Dot : constant UInt8 := 2#0010_0000#;
   begin
      if On then
         This.Buffer (I) := This.Buffer (I) or Dot;
      else
         This.Buffer (I) := This.Buffer (I) and not Dot;
      end if;
   end Set_Decimal;

   procedure Set
      (This : in out LCD_Wing;
       Pos  : Position;
       Ch   : Character)
   is
      Index : constant BU9796.Buffer_Index := BU9796.Buffer_Index (Pos) + 2;
   begin
      This.Buffer (Index) := (This.Buffer (Index) and 2#0010_0000#) or Lookup (Ch);
   end Set;

   procedure Set_Indicators
      (This  : in out LCD_Wing;
       Value : Indicators)
   is
   begin
      This.Buffer (2) := To_UInt8 (Value);
   end Set_Indicators;

   procedure Clear
      (This : in out LCD_Wing)
   is
   begin
      This.Buffer := (others => 0);
      This.Cursor := Position'First;
   end Clear;

   procedure Fill
      (This : in out LCD_Wing)
   is
   begin
      This.Buffer := (1 => 0, others => 16#FF#);
   end Fill;

   procedure Update
      (This : in out LCD_Wing)
   is
      Status : I2C_Status;
   begin
      This.Update (Status);
      if Status /= Ok then
         raise Bus_Error;
      end if;
   end Update;

   procedure Put
      (This : in out LCD_Wing;
       Ch   : Character)
   is
      function "-" (Left, Right : Character) return Character
      is (Character'Val (UInt8 (Character'Pos (Left)) - UInt8 (Character'Pos (Right))));

      function To_Upper (C : Character) return Character
      is (if C in 'a' .. 'z' then C - ('a' - 'A') else C);
   begin
      if This.Cursor = Position'First and Ch = '-' then
         --  The decimal for position 1 is actually a dash
         This.Set_Decimal (This.Cursor, True);
         return;
      end if;

      if This.Cursor = Position'First + 2 and Ch = ':' then
         This.Buffer (2) := This.Buffer (2) or To_UInt8 (Indicators'(Colon => True, others => False));
         return;
      end if;

      if This.Cursor > Position'First and Ch = '.' then
         This.Set_Decimal (This.Cursor, True);
         return;
      end if;

      This.Set (This.Cursor, To_Upper (Ch));

      if This.Cursor < Position'Last then
         This.Cursor := This.Cursor + 1;
      end if;
   end Put;

   procedure Put
      (This : in out LCD_Wing;
       S    : String)
   is
   begin
      for Ch of S loop
         This.Put (Ch);
      end loop;
   end Put;

end OSO;
