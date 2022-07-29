--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--

--  Standard LCD Segment Driver
--  https://fscdn.rohm.com/en/products/databook/datasheet/ic/driver/lcd_segment/bu9796axxx-e.pdf
--  https://www.digikey.com/en/products/detail/rohm-semiconductor/BU9796AMUV-E2/5253944
with HAL.I2C; use HAL.I2C;
with HAL; use HAL;

package BU9796 is

   subtype Buffer_Index is Integer range 1 .. 8;

   Default_Addr : constant I2C_Address := 16#3E#;

   type Device
      (Port : not null Any_I2C_Port;
       Addr : I2C_Address)
   is tagged record
      Timeout : Natural := 1_000;
      Buffer  : UInt8_Array (Buffer_Index) := (others => 0);
      --  Index 1 of Buffer is the Address Set command, which should always be
      --  zero for this implementation.
   end record;

   procedure Initialize
      (This   : in out Device;
       Status : out I2C_Status);

   procedure Write_Command
      (This    : in out Device;
       Cmd     : UInt8;
       Status  : out I2C_Status);

   procedure Update
      (This   : in out Device;
       Status : out I2C_Status);

end BU9796;
