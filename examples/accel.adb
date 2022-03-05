--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with Ada.Text_IO;

with Text_Format; use Text_Format;
with HAL.SPI;
with HAL;

with RP.Clock;
with RP.Device;
with RP.GPIO;
with RP.SPI;
with Pico;

with ADXL345;

procedure Accel is
   Port : RP.SPI.SPI_Port renames RP.Device.SPI_0;
   MISO : RP.GPIO.GPIO_Point renames Pico.GP0; --  SDO/ALT
   CS   : RP.GPIO.GPIO_Point renames Pico.GP1; --  ~CS
   SCK  : RP.GPIO.GPIO_Point renames Pico.GP2; --  SCL/SCLK
   MOSI : RP.GPIO.GPIO_Point renames Pico.GP3; --  SDA/SDI/SDIO
begin
   RP.Clock.Initialize (Pico.XOSC_Frequency);
   RP.Device.Timer.Enable;

   declare
      use RP.GPIO;
      use RP.SPI;
      Config : constant SPI_Configuration :=
         (Baud     => 1_000_000,
          Polarity => Active_High,  --  CPOL = 1
          Phase    => Falling_Edge, --  CPHA = 1
          others   => <>);
   begin
      Pico.LED.Configure (Output);
      MOSI.Configure (Output, Floating, SPI);
      MISO.Configure (Output, Floating, SPI);
      SCK.Configure (Output, Floating, SPI);
      CS.Configure (Output, Pull_Up);
      CS.Set;
      Port.Configure (Config);
   end;

   declare
      use Ada.Text_IO;
      use HAL;
      Device_Id : UInt8;
   begin
      if ADXL345.DEVID.Get (Port'Access, CS'Access, Device_Id)
         and then Device_Id = 2#11100101#
      then
         Pico.LED.Set;
         Put ("DEVID ");
         Put (Device_Id'Image);
         New_Line;
      else
         Pico.LED.Clear;
         Put_Line ("Device Id failed");
         return;
      end if;
   end;

   declare
      use Ada.Text_IO;
      use HAL;
      use ADXL345;
      Power  : POWER_CTL_Register := POWER_CTL.Get (Port'Access, CS'Access);
      Format : DATA_FORMAT_Register := DATA_FORMAT.Get (Port'Access, CS'Access);
      M : Measurement;
      G : Acceleration;
   begin
      Power.Measure := True;
      POWER_CTL.Set (Port'Access, CS'Access, Power);
      Power := POWER_CTL.Get (Port'Access, CS'Access);
      Put_Line ("Link       " & Power.Link'Image);
      Put_Line ("AUTO_SLEEP " & Power.AUTO_SLEEP'Image);
      Put_Line ("Measure    " & Power.Measure'Image);
      Put_Line ("Sleep      " & Power.Sleep'Image);
      Put_Line ("Wakeup     " & Power.Wakeup'Image);

      Format.G_Range := Range_2g;
      Format.Justify := True;
      Format.FULL_RES := True;
      DATA_FORMAT.Set (Port'Access, CS'Access, Format);

      loop
         if DATA.Get (Port'Access, CS'Access, M) then
            Put ("X=");
            G := To_Acceleration (Range_2g, M (X));
            Put (From_Float (G, Aft => 6));

            Put (" Y=");
            G := To_Acceleration (Range_2g, M (Y));
            Put (From_Float (G, Aft => 6));

            Put (" Z=");
            G := To_Acceleration (Range_2g, M (Z));
            Put (From_Float (G, Aft => 6));

            New_Line;
         end if;
         RP.Device.Timer.Delay_Seconds (1);
      end loop;
   end;
end Accel;
