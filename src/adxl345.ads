--
--  Copyright (C) 2022 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
with HAL; use HAL;

generic
   with procedure Read_Register  (Reg : UInt8; Val : out UInt8_Array);
   with procedure Write_Register (Reg : UInt8; Val : UInt8_Array);
package ADXL345 is

   type Int16 is range -2 ** 15 .. (2 ** 15 - 1)
      with Size => 16;

   procedure Initialize;

   procedure Read_Raw
      (X, Y, Z : out Int16);

   procedure Read
      (X, Y, Z : out Float);

   type Register is
      (DEVID,
       THRESH_TAP,
       OFSX,
       OFSY,
       OFSZ,
       DUR,
       Latent,
       Window,
       THRESH_ACT,
       THRESH_INACT,
       TIME_INACT,
       ACT_INACT_CTL,
       THRESH_FF,
       TIME_FF,
       TAP_AXES,
       ACT_TAP_STATUS,
       BW_RATE,
       POWER_CTL,
       INT_ENABLE,
       INT_MAP,
       INT_SOURCE,
       DATA_FORMAT,
       DATAX0,
       DATAX1,
       DATAY0,
       DATAY1,
       DATAZ0,
       DATAZ1,
       FIFO_CTL,
       FIFO_STATUS)
   with Size => 8;

   for Register use
      (DEVID            => 16#00#,
       THRESH_TAP       => 16#1D#,
       OFSX             => 16#1E#,
       OFSY             => 16#1F#,
       OFSZ             => 16#20#,
       DUR              => 16#21#,
       Latent           => 16#22#,
       Window           => 16#23#,
       THRESH_ACT       => 16#24#,
       THRESH_INACT     => 16#25#,
       TIME_INACT       => 16#26#,
       ACT_INACT_CTL    => 16#27#,
       THRESH_FF        => 16#28#,
       TIME_FF          => 16#29#,
       TAP_AXES         => 16#2A#,
       ACT_TAP_STATUS   => 16#2B#,
       BW_RATE          => 16#2C#,
       POWER_CTL        => 16#2D#,
       INT_ENABLE       => 16#2E#,
       INT_MAP          => 16#2F#,
       INT_SOURCE       => 16#30#,
       DATA_FORMAT      => 16#31#,
       DATAX0           => 16#32#,
       DATAX1           => 16#33#,
       DATAY0           => 16#34#,
       DATAY1           => 16#35#,
       DATAZ0           => 16#36#,
       DATAZ1           => 16#37#,
       FIFO_CTL         => 16#38#,
       FIFO_STATUS      => 16#39#);

   type TAP_AXES_Register is record
      Suppress : Boolean := False;
      TAP_X    : Boolean := False;
      TAP_Y    : Boolean := False;
      TAP_Z    : Boolean := False;
   end record
      with Size => 8;
   for TAP_AXES_Register use record
      Suppress at 0 range 3 .. 3;
      TAP_X    at 0 range 2 .. 2;
      TAP_Y    at 0 range 1 .. 1;
      TAP_Z    at 0 range 0 .. 0;
   end record;

   type ACT_TAP_STATUS_Register is record
      ACT_X    : Boolean := False;
      ACT_Y    : Boolean := False;
      ACT_Z    : Boolean := False;
      Asleep   : Boolean := False;
      TAP_X    : Boolean := False;
      TAP_Y    : Boolean := False;
      TAP_Z    : Boolean := False;
   end record
      with Size => 8;
   for ACT_TAP_STATUS_Register use record
      ACT_X    at 0 range 6 .. 6;
      ACT_Y    at 0 range 5 .. 5;
      ACT_Z    at 0 range 4 .. 4;
      Asleep   at 0 range 3 .. 3;
      TAP_X    at 0 range 2 .. 2;
      TAP_Y    at 0 range 1 .. 1;
      TAP_Z    at 0 range 0 .. 0;
   end record;

   type BW_RATE_Register is record
      LOW_POWER   : Boolean := False;
      Rate        : UInt4 := 2#1010#;
   end record
      with Size => 8;
   for BW_RATE_Register use record
      LOW_POWER   at 0 range 4 .. 4;
      Rate        at 0 range 0 .. 3;
   end record;

   type POWER_CTL_Wakeup_Field is
      (Wake_8_Hz, Wake_4_Hz, Wake_2_Hz, Wake_1_Hz)
      with Size => 2;
   type POWER_CTL_Register is record
      Link        : Boolean;
      AUTO_SLEEP  : Boolean;
      Measure     : Boolean;
      Sleep       : Boolean;
      Wakeup      : POWER_CTL_Wakeup_Field;
   end record
      with Size => 8;
   for POWER_CTL_Register use record
      Link        at 0 range 5 .. 5;
      AUTO_SLEEP  at 0 range 4 .. 4;
      Measure     at 0 range 3 .. 3;
      Sleep       at 0 range 2 .. 2;
      Wakeup      at 0 range 0 .. 1;
   end record;

   type INT_Register is record
      DATA_READY  : Boolean := False;
      SINGLE_TAP  : Boolean := False;
      DOUBLE_TAP  : Boolean := False;
      Activity    : Boolean := False;
      Inactivity  : Boolean := False;
      FREE_FALL   : Boolean := False;
      Watermark   : Boolean := False;
      Overrun     : Boolean := False;
   end record
      with Size => 8;
   for INT_Register use record
      DATA_READY  at 0 range 7 .. 7;
      SINGLE_TAP  at 0 range 6 .. 6;
      DOUBLE_TAP  at 0 range 5 .. 5;
      Activity    at 0 range 4 .. 4;
      Inactivity  at 0 range 3 .. 3;
      FREE_FALL   at 0 range 2 .. 2;
      Watermark   at 0 range 1 .. 1;
      Overrun     at 0 range 0 .. 0;
   end record;
   subtype INT_ENABLE_Register is INT_Register;
   subtype INT_MAP_Register is INT_Register;
   subtype INT_SOURCE_Register is INT_Register;

   type DATA_FORMAT_Register is record
      SELF_TEST   : Boolean := False;
      SPI         : Boolean := False;
      INT_INVERT  : Boolean := False;
      FULL_RES    : Boolean := False;
      Justify     : Boolean := False;
      G_Range     : UInt2 := 0;
   end record
      with Size => 8;
   for DATA_FORMAT_Register use record
      SELF_TEST   at 0 range 7 .. 7;
      SPI         at 0 range 6 .. 6;
      INT_INVERT  at 0 range 5 .. 5;
      FULL_RES    at 0 range 3 .. 3;
      Justify     at 0 range 2 .. 2;
      G_Range     at 0 range 0 .. 1;
   end record;

   type FIFO_CTL_FIFO_MODE_Field is (Bypass, FIFO, Stream, Trigger)
      with Size => 2;
   type FIFO_CTL_Register is record
      FIFO_MODE   : FIFO_CTL_FIFO_MODE_Field := Bypass;
      Trigger     : Boolean := False;
      Samples     : UInt5 := 0;
   end record
      with Size => 8;
   for FIFO_CTL_Register use record
      FIFO_MODE   at 0 range 6 .. 7;
      Trigger     at 0 range 5 .. 5;
      Samples     at 0 range 0 .. 4;
   end record;

   type FIFO_STATUS_Register is record
      FIFO_TRIG   : Boolean := False;
      Entries     : UInt6 := 0;
   end record
      with Size => 8;
   for FIFO_STATUS_Register use record
      FIFO_TRIG   at 0 range 7 .. 7;
      Entries     at 0 range 0 .. 5;
   end record;

end ADXL345;
