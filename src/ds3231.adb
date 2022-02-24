--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body DS3231 is
   use HAL.Real_Time_Clock;
   use HAL;

   procedure Read_All
      (This : DS3231_Device;
       Val  : out All_Registers)
   is
      use HAL.I2C;
      Data   : constant I2C_Data (1 .. 1) := (1 => 16#00#);
      Status : I2C_Status;
   begin
      --  Set the read address to zero
      This.I2C_Port.Master_Transmit (This.I2C_Address, Data, Status);
      if Status /= Ok then
         raise DS3231_Error with "Failed to set register address";
      end if;

      This.I2C_Port.Master_Receive (This.I2C_Address, Val, Status);
      if Status /= Ok then
         raise DS3231_Error with "Failed to read register values";
      end if;
   end Read_All;

   function To_Integer
      (BCD : HAL.UInt8)
      return Integer
   is
      N : constant Integer :=
         Integer (BCD and 16#F#) +
         (Integer (Shift_Right (BCD, 4)) * 10);
   begin
      return N;
   end To_Integer;

   function To_BCD
      (N : Integer)
      return HAL.UInt8
   is (Shift_Left (UInt8 (N / 10), 4) or UInt8 (N mod 10));

   overriding
   procedure Set
      (This : in out DS3231_Device;
       Time : RTC_Time;
       Date : RTC_Date)
   is
      use HAL.I2C;
      Val    : All_Registers := (others => 0);
      Data   : I2C_Data (1 .. 2) := (0, 0);
      Status : I2C_Status;
   begin
      Val (0) := To_BCD (Integer (Time.Sec));
      Val (1) := To_BCD (Integer (Time.Min));
      Val (2) := To_BCD (Integer (Time.Hour));
      Val (3) := UInt8 (RTC_Day_Of_Week'Pos (Date.Day_Of_Week));
      Val (4) := To_BCD (Integer (Date.Day));
      Val (5) := To_BCD (RTC_Month'Pos (Date.Month));
      Val (6) := To_BCD (Integer (Date.Year));
      --  Alarm registers exist here, but we don't use them.

      --  Overwrite the control register with default values
      Val (16#0E#) := 2#00011100#;

      for I in Val'Range loop
         Data := (UInt8 (I), Val (I));
         This.I2C_Port.Master_Transmit (This.I2C_Address, Data, Status);
         if Status /= Ok then
            raise DS3231_Error with "Failed to set register values";
         end if;
      end loop;
   end Set;

   overriding
   procedure Get
      (This : in out DS3231_Device;
       Time : out RTC_Time;
       Date : out RTC_Date)
   is
      Val : All_Registers;
      N   : Integer;
   begin
      --  If there's anything wrong with the I2C interface or RTC, there's a
      --  good chance we'll throw Constraint_Error here.
      Read_All (This, Val);
      Time.Sec   := RTC_Second (To_Integer (Val (16#00#)));
      Time.Min   := RTC_Minute (To_Integer (Val (16#01#)));
      Time.Hour  := RTC_Hour (To_Integer (Val (16#02#)));
      Date.Day   := RTC_Day (To_Integer (Val (16#04#)));

      --  Sometimes the RTC returns Day and Month as 0 right after power on.
      --  These conditionals paper over that rather than throwing an exception
      --  on the first read.
      N := Integer (Val (16#03#));
      if N not in 1 .. 7 then
         Date.Day_Of_Week := RTC_Day_Of_Week'First;
      else
         Date.Day_Of_Week := RTC_Day_Of_Week'Val (N);
      end if;

      N := Integer (Val (16#05#) and 16#7F#);
      if N not in 1 .. 31 then
         Date.Month := RTC_Month'First;
      else
         Date.Month := RTC_Month'Val (N);
      end if;

      Date.Year  := RTC_Year (To_Integer (Val (16#06#)));
   end Get;

   overriding
   function Get_Time
      (This : DS3231_Device)
      return RTC_Time
   is
      Val  : All_Registers;
      Time : RTC_Time;
   begin
      Read_All (This, Val);
      Time.Sec   := RTC_Second (To_Integer (Val (16#00#)));
      Time.Min   := RTC_Minute (To_Integer (Val (16#01#)));
      Time.Hour  := RTC_Hour (To_Integer (Val (16#02#)));
      return Time;
   end Get_Time;

   overriding
   function Get_Date
      (This : DS3231_Device)
      return RTC_Date
   is
      Val  : All_Registers;
      Date : RTC_Date;
   begin
      Read_All (This, Val);
      Date.Day   := RTC_Day (To_Integer (Val (16#04#)));
      Date.Day_Of_Week := RTC_Day_Of_Week'Val (Integer (Val (16#03#)));
      Date.Month := RTC_Month'Val (To_Integer (Val (16#05#) and 16#7F#));
      Date.Year  := RTC_Year (To_Integer (Val (16#06#)));
      return Date;
   end Get_Date;

end DS3231;
