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

with HAL; use HAL;
with HAL.I2C;

package BMP280 is

   type Device
      (Port : not null HAL.I2C.Any_I2C_Port;
       Addr : HAL.I2C.I2C_Address)
   is tagged private;

   type Pascals is new Float;
   type Celsius is new Float;

   procedure Initialize
      (This : in out Device);

   function Pressure
      (This : in out Device)
      return Pascals;

   function Temperature
      (This : in out Device)
      return Celsius;

private

   type Int16 is range -2 ** 15 .. 2 ** 15 - 1
      with Size => 16;

   type Device
      (Port : not null HAL.I2C.Any_I2C_Port;
       Addr : HAL.I2C.I2C_Address)
   is tagged record
      --  Calibration data follows
      dig_T1 : UInt16;
      dig_T2 : Int16;
      dig_T3 : Int16;
      dig_P1 : UInt16;
      dig_P2 : Int16;
      dig_P3 : Int16;
      dig_P4 : Int16;
      dig_P5 : Int16;
      dig_P6 : Int16;
      dig_P7 : Int16;
      dig_P8 : Int16;
      dig_P9 : Int16;
   end record;

   procedure Write_Register
      (This : in out Device;
       Addr : UInt8;
       Data : UInt8);

   procedure Read_Registers
      (This : in out Device;
       Addr : UInt8;
       Data : out HAL.I2C.I2C_Data);

   function To_Celsius
      (This : in out Device;
       Data : HAL.I2C.I2C_Data)
       return Celsius;

   function To_Pascals
      (This : in out Device;
       Data : HAL.I2C.I2C_Data)
       return Pascals;

end BMP280;
