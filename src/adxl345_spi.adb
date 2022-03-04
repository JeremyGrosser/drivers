with Ada.Unchecked_Conversion;

package body ADXL345_SPI is

   package body Register is
      use HAL.SPI;
      use HAL;

      subtype Data_Array is SPI_Data_8b (1 .. Register_Type'Size / 8);
      function To_Data_Array is new Ada.Unchecked_Conversion
         (Source => Register_Type, Target => Data_Array);
      function To_Register_Type is new Ada.Unchecked_Conversion
         (Source => Data_Array, Target => Register_Type);

      function Get
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : out Register_Type)
          return Boolean
      is
         Buffer : SPI_Data_8b (1 .. 1) := (1 => Address or 2#1000_0000#); --  read
         Data   : Data_Array := (others => 0);
         Status : SPI_Status;
      begin
         if Data'Length > 1 then
            Buffer (1) := Buffer (1) or 2#0100_0000#; --  multibyte
         end if;
         CS.Clear;
         Port.Transmit (Buffer, Status, Timeout => 0);
         Port.Receive (Buffer, Status, Timeout => 1);

         for I in Data'Range loop
            Buffer (1) := 0;
            Port.Transmit (Buffer, Status, Timeout => 0);
            if Status /= Ok then
               exit;
            end if;

            Port.Receive (Buffer, Status, Timeout => 1);
            if Status /= Ok then
               exit;
            end if;
            Data (I) := Buffer (1);
         end loop;
         CS.Set;
         Value := To_Register_Type (Data);
         return Status = Ok;
      end Get;

      function Get
         (Port : not null HAL.SPI.Any_SPI_Port;
          CS   : not null HAL.GPIO.Any_GPIO_Point)
          return Register_Type
      is
         V : Register_Type;
      begin
         if Get (Port, CS, V) then
            return V;
         else
            raise Program_Error with "ADXL345 SPI Read Error";
         end if;
      end Get;

      function Set
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : Register_Type)
          return Boolean
      is
         Buffer : SPI_Data_8b (1 .. 1) := (1 => Address);
         Data   : Data_Array := To_Data_Array (Value);
         Status : SPI_Status;
      begin
         if Data'Length > 1 then
            Buffer (1) := Buffer (1) or 2#0100_0000#; --  multibyte
         end if;
         CS.Clear;
         Port.Transmit (Buffer, Status, Timeout => 0);
         Port.Receive (Buffer, Status, Timeout => 1);

         for I in Data'Range loop
            Port.Transmit (Data (I .. I), Status, Timeout => 0);
            if Status /= Ok then
               exit;
            end if;

            Port.Receive (Data (I .. I), Status, Timeout => 1);
            if Status /= Ok then
               exit;
            end if;
         end loop;
         CS.Set;
         return Status = Ok;
      end Set;

      procedure Set
         (Port  : not null HAL.SPI.Any_SPI_Port;
          CS    : not null HAL.GPIO.Any_GPIO_Point;
          Value : Register_Type)
      is
      begin
         if Set (Port, CS, Value) = False then
            raise Program_Error with "ADXL345 SPI Write Error";
         end if;
      end Set;
   end Register;

end ADXL345_SPI;
