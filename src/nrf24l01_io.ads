with Ada.Unchecked_Conversion;
with HAL.GPIO;
with HAL.SPI;
with HAL; use HAL;

package NRF24L01_IO is

   type Pins is record
      Port : HAL.SPI.Any_SPI_Port;
      CS   : HAL.GPIO.Any_GPIO_Point;
      CE   : HAL.GPIO.Any_GPIO_Point;
      IRQ  : HAL.GPIO.Any_GPIO_Point;
   end record;

   type STATUS_Register is record
      RX_DR    : Boolean := False;
      TX_DS    : Boolean := False;
      MAX_RT   : Boolean := False;
      RX_P_NO  : UInt3   := 0;
      TX_FULL  : Boolean := False;
   end record
      with Size => 8;
   for STATUS_Register use record
      RX_DR    at 0 range 6 .. 6;
      TX_DS    at 0 range 5 .. 5;
      MAX_RT   at 0 range 4 .. 4;
      RX_P_NO  at 0 range 1 .. 3;
      TX_FULL  at 0 range 0 .. 0;
   end record;

   function To_STATUS_Register is new Ada.Unchecked_Conversion
      (Source => UInt8,
       Target => STATUS_Register);

   type IO_Status is record
      Bus : HAL.SPI.SPI_Status;
      Dev : STATUS_Register;
   end record;

   function Is_Error
      (Status : IO_Status)
      return Boolean;

   IO_Error : exception;

   procedure SPI_Transfer
      (P      : Pins;
       Cmd    : HAL.UInt8;
       Data   : in out HAL.SPI.SPI_Data_8b;
       Status : in out IO_Status);

   procedure Flush_RX (P : Pins);
   procedure Flush_TX (P : Pins);

   function Get_Status
      (P : Pins)
      return STATUS_Register;

   generic
      Address : UInt8;
      type Register_Type is private;
   package Register is

      procedure Read
         (P      : Pins;
          Value  : out Register_Type;
          Status : in out IO_Status);

      procedure Write
         (P      : Pins;
          Value  : Register_Type;
          Status : in out IO_Status);

      --  These functions throw exceptions in the event of an error, rather
      --  than setting an out parameter.
      function Read
         (P : Pins)
         return Register_Type;

      procedure Write
         (P      : Pins;
          Value  : Register_Type;
          Verify : Boolean := False);

   private

      subtype Register_Array is HAL.SPI.SPI_Data_8b (1 .. Register_Type'Size / 8);

      function To_Register_Array is new Ada.Unchecked_Conversion
         (Source => Register_Type,
          Target => Register_Array);

      function From_Register_Array is new Ada.Unchecked_Conversion
         (Source => Register_Array,
          Target => Register_Type);

      procedure Read
         (P      : Pins;
          Value  : in out Register_Array;
          Status : in out IO_Status);

      procedure Write
         (P      : Pins;
          Value  : in out Register_Array;
          Status : in out IO_Status);

   end Register;

end NRF24L01_IO;
