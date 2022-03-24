with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package SGP30 is

   Default_I2C_Address : constant I2C_Address := 16#B0#;

   type Device
      (Port : not null Any_I2C_Port;
       Addr : I2C_Address)
   is tagged null record;

   SGP30_Error : exception;

   procedure Soft_Reset
      (This : Device);
   --  Soft_Reset is a broadcast and will reset all devices on the bus that
   --  respond to General Call commands.

   procedure Init_Air_Quality
      (This : Device);

   procedure Measure_Air_Quality
      (This : Device;
       eCO2 : out Natural;
       TVOC : out Natural);

   function Get_Baseline
      (This : Device)
      return Natural;

   procedure Set_Baseline
      (This     : Device;
       Baseline : Natural);

   procedure Set_Humidity
      (This     : Device;
       Humidity : UInt16);

   function Measure_Test
      (This : Device)
      return UInt16;

   function Get_Feature_Set_Version
      (This : Device)
      return UInt16;

   function Measure_Raw_Signals
      (This : Device)
      return Natural;

   function Get_Serial_Id
      (This : Device)
      return UInt48;

private

   function Verify_Checksum
      (Data : UInt8_Array)
      return Boolean;

   function Read_48
      (This : Device;
       Reg  : UInt16)
       return UInt48;

   function Read_32
      (This : Device;
       Reg  : UInt16)
       return UInt32;

   function Read_16
      (This : Device;
       Reg  : UInt16)
       return UInt16;

   procedure Write_48
      (This  : Device;
       Reg   : UInt16;
       Value : UInt48);

   procedure Write_32
      (This  : Device;
       Reg   : UInt16;
       Value : UInt32);

   procedure Write_16
      (This  : Device;
       Reg   : UInt16;
       Value : UInt16);

   procedure Write_Command
      (This : Device;
       Reg  : UInt16);

end SGP30;
