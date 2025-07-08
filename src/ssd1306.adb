--
--  Copyright (C) 2024 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: MIT
--
pragma Style_Checks ("M120");

package body SSD1306 is

   MEMORYMODE           : constant UInt8 := 16#20#;
   COLUMNADDR           : constant UInt8 := 16#21#;
   PAGEADDR             : constant UInt8 := 16#22#;
   SETCONTRAST          : constant UInt8 := 16#81#;
   CHARGEPUMP           : constant UInt8 := 16#8D#;
   SEGREMAP             : constant UInt8 := 16#A0#;
   DISPLAYALLON_RESUME  : constant UInt8 := 16#A4#;
   NORMALDISPLAY        : constant UInt8 := 16#A6#;
   SETMULTIPLEX         : constant UInt8 := 16#A8#;
   DISPLAYOFF           : constant UInt8 := 16#AE#;
   DISPLAYON            : constant UInt8 := 16#AF#;
   COMSCANDEC           : constant UInt8 := 16#C8#;
   SETDISPLAYOFFSET     : constant UInt8 := 16#D3#;
   SETDISPLAYCLOCKDIV   : constant UInt8 := 16#D5#;
   SETPRECHARGE         : constant UInt8 := 16#D9#;
   SETCOMPINS           : constant UInt8 := 16#DA#;
   SETVCOMDETECT        : constant UInt8 := 16#DB#;
   SETSTARTLINE         : constant UInt8 := 16#40#;
   DEACTIVATE_SCROLL    : constant UInt8 := 16#2E#;

   subtype Framebuffer is UInt8_Array (0 .. Width * Height / 8 - 1);
   type FB_Swap is mod 2;
   FB : array (FB_Swap) of Framebuffer;
   Current_FB : FB_Swap := 0;

   procedure Swap is
   begin
      Current_FB := Current_FB + 1;
      Clear;
   end Swap;

   procedure Write_Command
      (Data : UInt8_Array)
   is
      Buffer : UInt8_Array (1 .. 2);
   begin
      for D of Data loop
         Buffer (1) := 16#80#;
         Buffer (2) := D;
         Write (Buffer);
      end loop;
   end Write_Command;

   procedure Write_Data
      (Data : UInt8_Array)
   is
      Buffer : UInt8_Array (1 .. Data'Length + 1);
   begin
      Buffer (1) := 16#40#;
      Buffer (2 .. Buffer'Last) := Data;
      Write (Buffer);
   end Write_Data;

   Init : constant UInt8_Array :=
      (DISPLAYOFF,
       SETDISPLAYCLOCKDIV,
       16#80#,
       SETMULTIPLEX,
       Height - 1,
       SETDISPLAYOFFSET,
       16#00#,
       SETSTARTLINE or 0,
       CHARGEPUMP,
       16#14#,
       MEMORYMODE,
       16#00#,
       SEGREMAP or 1,
       COMSCANDEC,
       SETCOMPINS,
       16#02#,
       SETCONTRAST,
       16#8F#,
       SETPRECHARGE,
       16#F1#,
       SETVCOMDETECT,
       16#40#,
       DISPLAYALLON_RESUME,
       NORMALDISPLAY,
       DEACTIVATE_SCROLL,
       DISPLAYON);

   procedure Initialize is
   begin
      Clear;
      Write_Command (Init);
      Update;
   end Initialize;

   procedure Set_Pixel
      (X   : Column;
       Y   : Row;
       Set : Boolean)
   is
      Index  : constant Natural := (Natural (Y) / 8) * Width + Natural (X);
      Offset : constant Natural := Natural (Y) mod 8;
      Mask   : constant UInt8 := 2 ** Offset;
   begin
      if Set then
         FB (Current_FB) (Index) := FB (Current_FB) (Index) or Mask;
      else
         FB (Current_FB) (Index) := FB (Current_FB) (Index) and not Mask;
      end if;
   end Set_Pixel;

   procedure Update is
      subtype FB_Bytes is UInt8_Array (1 .. Width * Height / 8);
      Data : FB_Bytes
         with Import, Address => FB (Current_FB)'Address;
   begin
      Write_Command
         (UInt8_Array'(
            PAGEADDR,
            16#00#,     --  Page start address
            16#FF#,     --  Page end (not really)
            COLUMNADDR,
            16#00#,     --  Column start address
            Width - 1   --  Column end address
         ));
      Write_Data (Data);
   end Update;

   procedure Clear is
   begin
      FB (Current_FB) := (others => 0);
   end Clear;
end SSD1306;
