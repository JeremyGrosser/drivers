--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;

generic
   with procedure Read  (Reg : UInt8; Data : out UInt8_Array) is <>;
   with procedure Write (Reg : UInt8; Data :     UInt8_Array) is <>;
package ADXL345 is

   type Int16 is range -2**15 .. 2**15 - 1
      with Size => 16;
   type Measurement is record
      X, Y, Z : Int16;
   end record
      with Size => 16 * 3;

   procedure Configure
      (Watermark : UInt5);
   --  INT1 will go HIGH when Watermark number of samples are available to be
   --  read from the FIFO.

   procedure Read
      (M : out Measurement);

end ADXL345;
