--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body ST7789 is

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Register'Enum_Rep (Reg)), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      This.CS.Set;
   end Write;

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register;
       Data : HAL.UInt8)
   is
   begin
      Write (This, Reg, HAL.UInt8_Array'(1 => Data));
   end Write;

   procedure Write
      (This : in out ST7789_Screen;
       Reg  : Register;
       Data : HAL.UInt8_Array)
   is
      use HAL.SPI;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Clear;
      This.Port.Transmit (SPI_Data_8b'(1 => Register'Enum_Rep (Reg)), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;

      This.DC.Set;
      This.Port.Transmit (SPI_Data_8b (Data), Status);
      if Status /= Ok then
         raise Program_Error;
      end if;
      This.CS.Set;
   end Write;

   procedure Initialize
      (This : in out ST7789_Screen)
   is
      subtype U8 is HAL.UInt8_Array;
   begin
      This.RST.Clear;
      This.CS.Clear;
      This.Time.Delay_Milliseconds (50);
      This.RST.Set;
      This.CS.Set;
      This.Time.Delay_Milliseconds (50);

      Write (This, SWRESET);
      This.Time.Delay_Milliseconds (150);
      Write (This, MADCTL,    16#04#);
      Write (This, TEON,      16#00#);
      Write (This, FRMCTR2,   U8'(16#0C#, 16#0C#, 16#00#, 16#33#, 16#33#));
      Write (This, COLMOD,    16#55#); --  16bpp RGB565 see datasheet 8.8.42
      Write (This, GAMSET,    16#04#);

      Write (This, GCTRL,     16#14#);
      Write (This, VCOMS,     16#25#);
      Write (This, LCMCTRL,   16#2C#);
      Write (This, VDVVRHEN,  16#01#);
      Write (This, VRHS,      16#12#);
      Write (This, VDVS,      16#20#);
      Write (This, PWRCTRL1,  U8'(16#A4#, 16#A1#));
      Write (This, FRCTRL2,   16#15#); --  50 Hz, column inversion
      Write (This, GMCTRP1,   U8'(16#D0#, 16#04#, 16#0D#, 16#11#, 16#13#, 16#2B#, 16#3F#,
                                  16#54#, 16#4C#, 16#18#, 16#0D#, 16#0B#, 16#1F#, 16#23#));
      Write (This, GMCTRN1,   U8'(16#D0#, 16#04#, 16#0C#, 16#11#, 16#13#, 16#2C#, 16#3F#,
                                  16#44#, 16#51#, 16#2F#, 16#1F#, 16#1F#, 16#20#, 16#23#));
      Write (This, INVON);
      Write (This, SLPOUT);
      Write (This, DISPON);
      This.Time.Delay_Milliseconds (100);
      Write (This, CASET, U8'(16#00#, 16#00#, 16#00#, 16#EF#));
      Write (This, RASET, U8'(16#00#, 16#00#, 16#00#, 16#EF#));
      Write (This, RAMWR);
   end Initialize;

   procedure Write
      (This : in out ST7789_Screen;
       Data : Pixels)
   is
      use HAL.SPI;
      D : SPI_Data_16b (1 .. Data'Length)
         with Import, Address => Data'Address;
      Status : SPI_Status;
   begin
      This.CS.Clear;
      This.DC.Set;
      This.Port.Transmit (D, Status, Timeout => 0);
      This.CS.Set;
   end Write;

end ST7789;
