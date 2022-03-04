with HAL.GPIO;
with HAL.SPI;
with HAL;

package ADXL345_SPI is

   generic
      Address : HAL.UInt8;
      type Register_Type is private;
   package Register is
      function Get
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : out Register_Type)
          return Boolean;
      --  Returns False if the read failed

      function Get
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point)
          return Register_Type;
      --  Raises Program_Error if the read failed

      function Set
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : Register_Type)
          return Boolean;
      --  Returns False if the write failed

      procedure Set
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : Register_Type);
      --  Raises Program_Error if the write failed
   end Register;

end ADXL345_SPI;
