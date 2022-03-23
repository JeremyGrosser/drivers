--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with RP.GPIO;
with RP.PWM;
with RP;

package Piezo is

   type Beeper
      (Point_A : access RP.GPIO.GPIO_Point;
       Point_B : access RP.GPIO.GPIO_Point)
   is tagged record
      Slice : RP.PWM.PWM_Slice := RP.PWM.To_PWM (Point_A.all).Slice;
   end record;

   subtype Milliseconds is Natural;

   --  Timer and PWM must be initialized before this.
   procedure Beep
      (This      : Beeper;
       Duration  : Milliseconds := 1_000;
       Frequency : RP.Hertz := 440;
       Count     : Positive := 1);

end Piezo;
