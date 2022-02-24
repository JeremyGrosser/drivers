--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL.Real_Time_Clock;
with HAL.I2C;
with HAL;

package DS3231 is

   Default_Address : constant HAL.I2C.I2C_Address := 2#1101000#;
   type DS3231_Device
      (I2C_Port    : not null HAL.I2C.Any_I2C_Port;
       I2C_Address : HAL.I2C.I2C_Address)
   is limited new HAL.Real_Time_Clock.RTC_Device with private;

   overriding
   procedure Set
      (This : in out DS3231_Device;
       Time : HAL.Real_Time_Clock.RTC_Time;
       Date : HAL.Real_Time_Clock.RTC_Date);

   overriding
   procedure Get
      (This : in out DS3231_Device;
       Time : out HAL.Real_Time_Clock.RTC_Time;
       Date : out HAL.Real_Time_Clock.RTC_Date);

   overriding
   function Get_Time
      (This : DS3231_Device)
      return HAL.Real_Time_Clock.RTC_Time;

   overriding
   function Get_Date
      (This : DS3231_Device)
      return HAL.Real_Time_Clock.RTC_Date;

   --  Raised if any I2C error occurs
   DS3231_Error : exception;

private

   type DS3231_Device
      (I2C_Port    : not null HAL.I2C.Any_I2C_Port;
       I2C_Address : HAL.I2C.I2C_Address)
   is limited new HAL.Real_Time_Clock.RTC_Device with null record;

   subtype All_Registers is HAL.UInt8_Array (16#00# .. 16#12#);

   procedure Read_All
      (This : DS3231_Device;
       Val  : out All_Registers);

   function To_Integer
      (BCD : HAL.UInt8)
      return Integer;

   function To_BCD
      (N : Integer)
      return HAL.UInt8;

end DS3231;
