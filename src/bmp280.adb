--  BMP280 pressure and temperature sensor services

--  Copyright (C)2016-2021, Philip Munts, President, Munts AM Corp.
--  Copyright (C)2022, Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-1-Clause
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.

with Ada.Unchecked_Conversion;

package body BMP280 is
   use HAL.I2C;

   REG_CALIB0  : constant UInt8 := 16#88#;
   REG_ID      : constant UInt8 := 16#D0#;
   REG_RESET   : constant UInt8 := 16#E0#;
   REG_STATUS  : constant UInt8 := 16#F3#;
   REG_CONTROL : constant UInt8 := 16#F4#;
   REG_CONFIG  : constant UInt8 := 16#F5#;
   REG_PMSB    : constant UInt8 := 16#F7#;
   REG_PLSB    : constant UInt8 := 16#F8#;
   REG_PXLSB   : constant UInt8 := 16#F9#;
   REG_TMSB    : constant UInt8 := 16#FA#;
   REG_TLSB    : constant UInt8 := 16#FB#;
   REG_TXLSB   : constant UInt8 := 16#FC#;

   --  Control register settings
   CONTROL_SLEEP  : constant UInt8 := 2#001_001_00#; --  Sleep mode
   CONTROL_SAMPLE : constant UInt8 := 2#001_001_01#; --  Force sample mode

   --  Write to a single BMP280 register
   procedure Write_Register
      (This : in out Device;
       Addr : UInt8;
       Data : UInt8)
   is
      Cmd    : constant I2C_Data (1 .. 2) := (Addr, Data);
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit (This.Addr, Cmd, Status);
   end Write_Register;

   --  Read from one or more BMP280 registers
   procedure Read_Registers
      (This : in out Device;
       Addr : UInt8;
       Data : out I2C_Data)
   is
      Status : I2C_Status;
      Cmd    : constant I2C_Data (1 .. 1) := (1 => Addr);
   begin
      This.Port.Master_Transmit (This.Addr, Cmd, Status);
      This.Port.Master_Receive (This.Addr, Data, Status);
   end Read_Registers;

   --  Define conversions for calibration data
   function To_Int16 is new Ada.Unchecked_Conversion
      (Source => UInt16,
       Target => Int16);

   function To_UInt16
      (LSB, MSB : UInt8)
      return UInt16
   is (UInt16 (LSB) + UInt16 (MSB) * 256);

   function To_Int16 (LSB, MSB : UInt8)
      return Int16
   is (To_Int16 (To_UInt16 (LSB, MSB)));

   procedure Initialize
      (This : in out Device)
   is
      Status           : I2C_Data (1 .. 1);
      Calibration_Data : I2C_Data (1 .. 26);
   begin
      This.Write_Register (REG_CONTROL, CONTROL_SLEEP);
      This.Write_Register (REG_CONFIG, 2#0000_0000#);

      --  Wait while the BMP280 is busy
      loop
         This.Read_Registers (REG_STATUS, Status);
         exit when (Status (1) and 16#03#) = 0;
      end loop;

      --  Read calibration data
      This.Read_Registers (REG_CALIB0, Calibration_Data);

      --  Extract calibration data
      This.dig_T1 := To_UInt16 (Calibration_Data (1), Calibration_Data (2));
      This.dig_T2 := To_Int16 (Calibration_Data (3), Calibration_Data (4));
      This.dig_T3 := To_Int16 (Calibration_Data (5), Calibration_Data (6));
      This.dig_P1 := To_UInt16 (Calibration_Data (7), Calibration_Data (8));
      This.dig_P2 := To_Int16 (Calibration_Data (9), Calibration_Data (10));
      This.dig_P3 := To_Int16 (Calibration_Data (11), Calibration_Data (12));
      This.dig_P4 := To_Int16 (Calibration_Data (13), Calibration_Data (14));
      This.dig_P5 := To_Int16 (Calibration_Data (15), Calibration_Data (16));
      This.dig_P6 := To_Int16 (Calibration_Data (17), Calibration_Data (18));
      This.dig_P7 := To_Int16 (Calibration_Data (19), Calibration_Data (20));
      This.dig_P8 := To_Int16 (Calibration_Data (21), Calibration_Data (22));
      This.dig_P9 := To_Int16 (Calibration_Data (23), Calibration_Data (24));
   end Initialize;

   --  Convert 20-bit temperature sample to Celsius
   function To_Celsius
      (This : in out Device;
       Data : I2C_Data)
       return Celsius
   is
      adc_T   : Integer;
      A1      : Float;
      A2      : Float;
      C1      : Float;
      C2      : Float;
      C3      : Float;
      C4      : Float;
      var1    : Float;
      var2    : Float;
   begin
      --  // Returns temperature in DegC, double precision. Output value of “51.23” equals 51.23 DegC.
      --  double bmp280_compensate_T_double(BMP280_S32_t adc_T)
      --  {
      --    double var1, var2, T;
      --    var1 = (((double)adc_T)/16384.0 – ((double)dig_T1)/1024.0) * ((double)dig_T2);
      --    var2 = ((((double)adc_T)/131072.0 – ((double)dig_T1)/8192.0) *
      --      (((double)adc_T)/131072.0 – ((double) dig_T1)/8192.0)) * ((double)dig_T3);
      --    T = (var1 + var2) / 5120.0;
      --    return T;
      --  }
      adc_T := Integer (Data (4)) * 4096 + Integer (Data (5)) * 16 + Integer (Data (6)) / 16;
      A1 := Float (adc_T) / 16384.0;
      A2 := Float (adc_T) / 131072.0;

      C1 := Float (This.dig_T1) / 1024.0;
      C2 := Float (This.dig_T1) / 8192.0;
      C3 := Float (This.dig_T2);
      C4 := Float (This.dig_T3);

      var1 := (A1 - C1) * C3;
      var2 := (A2 - C2) * (A2 - C2) * C4;
      return Celsius ((var1 + var2) / 5120.0);
   end To_Celsius;

   --  Convert 20-bit pressure sample to Pascals
   function To_Pascals
      (This : in out Device;
       Data : I2C_Data)
       return Pascals
   is
      adc_P : Integer;
      var1  : Float;
      var2  : Float;
      p     : Float;
   begin
      --  // Returns pressure in Pa as double. Output value of “96386.2” equals 96386.2 Pa = 963.862 hPa
      --  double bmp280_compensate_P_double(BMP280_S32_t adc_P)
      --  {
      --    double var1, var2, p;
      --    var1 = ((double)t_fine/2.0) – 64000.0;
      --    var2 = var1 * var1 * ((double)dig_P6) / 32768.0;
      --    var2 = var2 + var1 * ((double)dig_P5) * 2.0;
      --    var2 = (var2/4.0)+(((double)dig_P4) * 65536.0);
      --    var1 = (((double)dig_P3) * var1 * var1 / 524288.0 + ((double)dig_P2) * var1) / 524288.0;
      --    var1 = (1.0 + var1 / 32768.0)*((double)dig_P1);
      --    if (var1 == 0.0)
      --    {
      --      return 0; // avoid exception caused by division by zero
      --    }
      --    p = 1048576.0 – (double)adc_P;
      --    p = (p – (var2 / 4096.0)) * 6250.0 / var1;
      --    var1 = ((double)dig_P9) * p * p / 2147483648.0;
      --    var2 = p * ((double)dig_P8) / 32768.0;
      --    p = p + (var1 + var2 + ((double)dig_P7)) / 16.0;
      --    return p;
      --  }
      adc_P := Integer (Data (1)) * 4096 + Integer (Data (2)) * 16 + Integer (Data (3)) / 16;
      var1 := Float (This.To_Celsius (Data)) * 2560.0 - 64000.0;
      var2 := var1 * var1 * Float (This.dig_P6) / 32768.0;
      var2 := var2 + var1 * Float (This.dig_P5) * 2.0;
      var2 := var2 / 4.0 + Float (This.dig_P4) * 65536.0;
      var1 := (Float (This.dig_P3) * var1 * var1 / 524288.0 + Float (This.dig_P2) * var1) / 524288.0;
      var1 := (1.0 + var1 / 32768.0) * Float (This.dig_P1);

      p    := 1048576.0 - Float (adc_P);
      p    := (p - (var2 / 4096.0)) * 6250.0 / var1;
      var1 := Float (This.dig_P9) * p * p / 2147483648.0;
      var2 := p * Float (This.dig_P8) / 32768.0;
      p    := p + (var1 + var2 + Float (This.dig_P7)) / 16.0;
      return Pascals (p);
   end To_Pascals;

   --  Read BMP280 pressure
   function Pressure
      (This : in out Device)
      return Pascals
   is
      Status : I2C_Data (1 .. 1);
      Data   : I2C_Data (1 .. 6);
   begin
      --  Initiate sampling
      This.Write_Register (REG_CONTROL, CONTROL_SAMPLE);

      --  Wait while the BMP280 is busy
      loop
         This.Read_Registers (REG_STATUS, Status);
         exit when (Status (1) and 16#03#) = 16#00#;
      end loop;

      --  Read sample data
      This.Read_Registers (REG_PMSB, Data);

      --  Convert to Pascals
      return This.To_Pascals (Data);
   end Pressure;

   --  Read BMP280 temperature
   function Temperature
      (This : in out Device)
      return Celsius
   is
      Status : I2C_Data (1 .. 1);
      Data   : I2C_Data (1 .. 6);
   begin
      --  Initiate sampling
      This.Write_Register (REG_CONTROL, CONTROL_SAMPLE);

      --  Wait while the BMP280 is busy
      loop
         This.Read_Registers (REG_STATUS, Status);
         exit when (Status (1) and 16#03#) = 16#00#;
      end loop;

      --  Read sample data
      This.Read_Registers (REG_PMSB, Data);

      --  Convert to Pascals
      return This.To_Celsius (Data);
   end Temperature;

end BMP280;
