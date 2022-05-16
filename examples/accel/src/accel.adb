--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;
with HAL.SPI;

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
   INT1 : RP.GPIO.GPIO_Point renames Pico.GP4;

   procedure SPI_Transfer
      (Data : in out UInt8)
   is
      use HAL.SPI;
      Status : SPI_Status;
      D : SPI_Data_8b (1 .. 1) := (1 => Data);
   begin
      Port.Transmit (D, Status, Timeout => 100);
      if Status /= Ok then
         raise Program_Error with "SPI transmit timeout";
      end if;
      Port.Receive (D, Status, Timeout => 100);
      if Status /= Ok then
         raise Program_Error with "SPI receive timeout";
      end if;
      Data := D (1);
   end SPI_Transfer;

   procedure SPI_Read
      (Reg  : UInt8;
       Data : out UInt8_Array)
   is
      D : UInt8;
   begin
      CS.Clear;
      D := Reg or 2#1000_0000#;
      SPI_Transfer (D);

      for I in Data'Range loop
         D := 0;
         SPI_Transfer (D);
         Data (I) := D;
      end loop;
      CS.Set;
   end SPI_Read;

   procedure SPI_Write
      (Reg  : UInt8;
       Data : UInt8_Array)
   is
      D : UInt8;
   begin
      CS.Clear;
      D := Reg;
      SPI_Transfer (D);

      for I in Data'Range loop
         D := Data (I);
         SPI_Transfer (D);
      end loop;
      CS.Set;
   end SPI_Write;

   package ADXL345_SPI is new ADXL345
      (Read  => SPI_Read,
       Write => SPI_Write);

   procedure Initialize is
      use RP.GPIO;
      use RP.SPI;
      Config : constant SPI_Configuration :=
         (Baud     => 5_000_000,
          Polarity => Active_High,  --  CPOL = 1
          Phase    => Falling_Edge, --  CPHA = 1
          others   => <>);
   begin
      RP.Clock.Initialize (Pico.XOSC_Frequency);
      RP.Device.Timer.Enable;

      MOSI.Configure (Output, Floating, RP.GPIO.SPI);
      MISO.Configure (Output, Floating, RP.GPIO.SPI);
      SCK.Configure (Output, Floating, RP.GPIO.SPI);
      CS.Configure (Output, Pull_Up);
      CS.Set;
      Port.Configure (Config);

      INT1.Configure (Input, Floating);
   end Initialize;

   use ADXL345_SPI;
   A : Measurement;
begin
   Initialize;

   Configure (Watermark => 1);
   loop
      if INT1.Get then
         Read (A);
      end if;
   end loop;
end Accel;
