--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
--  Plantower Particulate Matter Sensor
--  https://www.espruino.com/datasheets/PMS7003.pdf
--
--  This driver is compatible with all of the Plantower PM2.5 sensors:
--    PMS1003
--    PMS3003
--    PMS5003
--    PMS6003
--    PMS7003 (tested)
--    PMSA003
--
--  The module has a female socket that accepts 2x5 1.0mm pitch pins.
--  Samtec SFSD-05-28C-G-09.00-S is a compatible wire-to-board assembly.
--
--   ________---_
--  |  9 7 5 3 1 |
--  | 10 8 6 4 2 |
--   ‾‾‾‾‾‾‾‾‾‾‾‾
--
--  Pin  Function
--   1   +5V
--   2   +5V
--   3   GND
--   4   GND
--   5   RESET (active low)
--   6   NC
--   7   RX
--   8   NC
--   9   TX
--   10  SET (active low)
--
--  RESET and SET should have R10k pullups to your logic level (either 3.3 or 5.0V)
--  RX and TX are a 9600 8n1 UART
--
--  This driver only receives data from the sensor (active mode), it does not
--  send commands or support other modes.
--
--  Read this before trusting any data you get from this sensor.
--  https://learn.adafruit.com/pm25-air-quality-sensor/usage-notes
--
with HAL.UART; use HAL.UART;
with HAL; use HAL;

package PMS is

   type Field is
      (Start, Length,
       CF1_PM_1, CF1_PM_2_5, CF1_PM_10,
       ATM_PM_1, ATM_PM_2_5, ATM_PM_10,
       PART_0_3, PART_0_5, PART_1, PART_2_5, PART_5, PART_10,
       Reserved, Checksum);
   subtype Measurement is Field range CF1_PM_1 .. PART_10;

   type Frame is array (Field) of UInt16
      with Component_Size => 16;

   function Name
      (F : Field)
      return String;

   procedure Receive
      (Port    : not null Any_UART_Port;
       Data    : out Frame;
       Status  : out UART_Status;
       Timeout : Natural := 1_000);

   function Calculate_Checksum
      (Data : Frame)
      return UInt16;

end PMS;
