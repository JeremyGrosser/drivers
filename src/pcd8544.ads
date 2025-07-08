with HAL;

generic
   with procedure Write (Data : HAL.UInt8);
   with procedure Set_DC (High : Boolean);
   with procedure Set_RST (High : Boolean);
package PCD8544 is

   Width  : constant := 84;
   Height : constant := 48;

   procedure Initialize;

   procedure Set_Pixel
      (X, Y : Natural);

   procedure Clear_Screen;

   procedure Update;

end PCD8544;
