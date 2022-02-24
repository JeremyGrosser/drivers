--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
package body Seven is

   procedure Set_Digit
      (This : in out Device;
       Pos  : Position;
       Val  : Digit)
   is
   begin
      case Pos is
         when 1 =>
            This.Buffer (1) :=  Numbers (Val);
         when 2 =>
            This.Buffer (3) :=  Numbers (Val);
         when 3 =>
            This.Buffer (7) :=  Numbers (Val);
         when 4 =>
            This.Buffer (9) :=  Numbers (Val);
      end case;
   end Set_Digit;

   procedure Set_Colon
      (This : in out Device;
       On   : Boolean)
   is
   begin
      if On then
         This.Set (33);
      else
         This.Clear (33);
      end if;
   end Set_Colon;

   procedure Set_Point
      (This : in out Device;
       Num  : Point_Number;
       On   : Boolean)
   is
   begin
      if On then
         This.Set (Points (Num));
      else
         This.Clear (Points (Num));
      end if;
   end Set_Point;

end Seven;
