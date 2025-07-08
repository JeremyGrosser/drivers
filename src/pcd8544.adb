--
--  Copyright (C) 2025 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body PCD8544 is
   use HAL;

   type Row is range 0 .. Height - 1;
   type Column is range 0 .. Width - 1;

   type Framebuffer is array (Column, Row) of Boolean
      with Component_Size => 1;
   FB  : aliased Framebuffer;
   Raw : UInt8_Array (1 .. (Width * Height) / 8)
      with Address => FB'Address;

   procedure Initialize is
   begin
      Clear_Screen;

      Set_RST (False);
      Set_RST (True);
      Set_DC (False);

      Write (2#0010_0001#); --  power on, extended mode
      Write (2#0001_0011#); --  BS = 3
      Write (2#1100_0000#); --  VOP = 64
      Write (2#0010_0010#); --  basic mode, vertical addressing
      Write (2#0000_1100#); --  normal display
      --  Write (2#0100_0000#); --  y = 0
      --  Write (2#1000_0000#); --  x = 0
   end Initialize;

   procedure Update is
   begin
      Set_DC (True);
      for D of Raw loop
         Write (D);
      end loop;
   end Update;

   procedure Set_Pixel
      (X, Y : Natural)
   is
   begin
      FB (Width - 1 - Column (X), Height - 1 - Row (Y)) := True;
   end Set_Pixel;

   procedure Clear_Screen is
   begin
      FB := (others => (others => False));
   end Clear_Screen;

end PCD8544;
