--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body ADXL345 is
   function To_Acceleration
      (G_Range : DATA_FORMAT_G_Range_Field;
       A       : HAL.UInt16)
      return Acceleration
   is
      use HAL;
      F : Acceleration;
   begin
      F := Float (A and 16#7FFF#);
      if (A and 16#8000#) /= 0 then
         F := F * (-1.0);
      end if;
      F := F / Float (2 ** 15 - 1);
      case G_Range is
         when Range_2g =>
            F := F * 2.0;
         when Range_4g =>
            F := F * 4.0;
         when Range_8g =>
            F := F * 8.0;
         when Range_16g =>
            F := F * 16.0;
      end case;
      return F;
   end To_Acceleration;
end ADXL345;
