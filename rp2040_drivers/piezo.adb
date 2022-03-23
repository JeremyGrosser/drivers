--
--  Copyright (C) 2021 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with RP.Device;
with RP; use RP;

package body Piezo is

   procedure Beep
      (This      : Beeper;
       Duration  : Milliseconds := 1_000;
       Frequency : Hertz := 440;
       Count     : Positive := 1)
   is
      use RP.GPIO;
      use RP.PWM;
      Period : constant := 10_000;
      Half   : constant := Period / 2;
   begin
      if This.Point_A /= null then
         Configure (This.Point_A.all, Output, Floating, RP.GPIO.PWM);
      end if;

      if This.Point_B /= null then
         Configure (This.Point_B.all, Output, Floating, RP.GPIO.PWM);
      end if;

      Set_Mode (This.Slice, Free_Running);
      Set_Frequency (This.Slice, Frequency * Period);
      Set_Interval (This.Slice, Period);
      Set_Invert (This.Slice,
         Channel_A => False,
         Channel_B => True);
      Set_Duty_Cycle (This.Slice, 0, 0);
      Enable (This.Slice);

      for I in 1 .. Count loop
         Set_Duty_Cycle (This.Slice, Half, Half);
         RP.Device.Timer.Delay_Milliseconds (Duration);
         Set_Duty_Cycle (This.Slice, 0, 0);
         if I /= Count then
            RP.Device.Timer.Delay_Milliseconds (Duration);
         end if;
      end loop;
   end Beep;

end Piezo;
