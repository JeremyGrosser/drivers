--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.Time;
with HAL.GPIO;
with HAL.SPI;
with HAL;

package ST7789 is
   type ST7789_Screen
      (CS   : not null HAL.GPIO.Any_GPIO_Point;
       DC   : not null HAL.GPIO.Any_GPIO_Point;
       RST  : not null HAL.GPIO.Any_GPIO_Point;
       Port : not null HAL.SPI.Any_SPI_Port;
       Time : not null HAL.Time.Any_Delays)
   is tagged null record;

   --  These register names come from the ST7735S datasheet, just for extra confusion.
   type Register is
      (SWRESET,
       SLPOUT,
       INVOFF,
       INVON,
       GAMSET,
       DISPOFF,
       DISPON,
       CASET,
       RASET,
       RAMWR,
       TEON,
       MADCTL,
       COLMOD,
       FRMCTR1,
       FRMCTR2,
       GCTRL,
       VCOMS,
       LCMCTRL,
       VDVVRHEN,
       VRHS,
       VDVS,
       FRCTRL2,
       PWRCTRL1,
       GMCTRP1,
       GMCTRN1);

   procedure Initialize
      (This : in out ST7789_Screen);

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register);

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register;
       Data : HAL.UInt8);

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register;
       Data : HAL.UInt8_Array);

   type RGB565 is record
      R : HAL.UInt5;
      G : HAL.UInt6;
      B : HAL.UInt5;
   end record
      with Size => 16;

   for RGB565 use record
      R at 0 range 11 .. 15;
      G at 0 range 5 .. 10;
      B at 0 range 0 .. 4;
   end record;

   type Pixels is array (Integer range <>) of RGB565
      with Component_Size => 16;

   procedure Write
      (This : in out ST7789_Screen;
       Data : Pixels);

private

   for Register'Size use 8;
   for Register use
      (SWRESET   => 16#01#,
       SLPOUT    => 16#11#,
       INVOFF    => 16#20#,
       INVON     => 16#21#,
       GAMSET    => 16#26#,
       DISPOFF   => 16#28#,
       DISPON    => 16#29#,
       CASET     => 16#2A#,
       RASET     => 16#2B#,
       RAMWR     => 16#2C#,
       TEON      => 16#35#,
       MADCTL    => 16#36#,
       COLMOD    => 16#3A#,
       FRMCTR1   => 16#B1#,
       FRMCTR2   => 16#B2#,
       GCTRL     => 16#B7#,
       VCOMS     => 16#BB#,
       LCMCTRL   => 16#C0#,
       VDVVRHEN  => 16#C2#,
       VRHS      => 16#C3#,
       VDVS      => 16#C4#,
       FRCTRL2   => 16#C6#,
       PWRCTRL1  => 16#D0#,
       GMCTRP1   => 16#E0#,
       GMCTRN1   => 16#E1#);
end ST7789;
