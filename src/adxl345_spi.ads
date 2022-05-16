--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;
with HAL.GPIO;
with HAL.SPI;
with ADXL345;

generic
   Port : not null HAL.SPI.Any_SPI_Port;
   CS   : not null HAL.GPIO.Any_GPIO_Point;
package ADXL345_SPI is

   procedure SPI_Read
      (Reg : UInt8;
       Val : out UInt8_Array);

   procedure SPI_Write
      (Reg : UInt8;
       Val : UInt8_Array);

   package Device is new ADXL345
      (Read_Register  => SPI_Read,
       Write_Register => SPI_Write);

end ADXL345_SPI;
