--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.Time; use HAL.Time;
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package SGP30 is

   Default_I2C_Address : constant I2C_Address := 16#B0#;

   type SGP30_Status is (Ok, I2C_Error, Checksum_Error);

   type Device
      (Port   : not null Any_I2C_Port;
       Addr   : I2C_Address;
       Delays : not null Any_Delays)
   is tagged record
      Bus_Status : I2C_Status;
      Status     : SGP30_Status := Ok;
   end record;

   function Has_Error
      (This : Device)
      return Boolean;

   procedure Clear_Error
      (This : in out Device);

   procedure Soft_Reset
      (This : in out Device);
   --  Soft_Reset is a broadcast and will reset all devices on the bus that
   --  respond to General Call commands.

   procedure Init_Air_Quality
      (This : in out Device);

   procedure Measure_Air_Quality
      (This : in out Device;
       eCO2 : out Natural;
       TVOC : out Natural);

   function Get_Baseline
      (This : in out Device)
      return UInt32;

   procedure Set_Baseline
      (This     : in out Device;
       Baseline : UInt32);

   procedure Set_Humidity
      (This     : in out Device;
       Humidity : UInt16);

   function Measure_Test
      (This : in out Device)
      return UInt16;

   function Get_Feature_Set_Version
      (This : in out Device)
      return UInt16;

   function Measure_Raw_Signals
      (This : in out Device)
      return UInt32;

   function Get_Serial_Id
      (This : in out Device)
      return UInt48;

private

   function Verify_Checksum
      (Data : UInt8_Array)
      return Boolean;

   function Read_48
      (This : in out Device;
       Reg  : UInt16)
       return UInt48;

   function Read_32
      (This : in out Device;
       Reg  : UInt16)
       return UInt32;

   function Read_16
      (This : in out Device;
       Reg  : UInt16)
       return UInt16;

   procedure Write_32
      (This  : in out Device;
       Reg   : UInt16;
       Value : UInt32);

   procedure Write_16
      (This  : in out Device;
       Reg   : UInt16;
       Value : UInt16);

   procedure Write_Command
      (This : in out Device;
       Reg  : UInt16);

   function CRC_8
      (Data : UInt8_Array)
      return UInt8;

   function To_I2C_Data
      (X : UInt16)
      return I2C_Data;

end SGP30;
