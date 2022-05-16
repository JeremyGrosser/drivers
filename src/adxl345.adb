pragma Warnings (Off, "* is not referenced");
with Ada.Unchecked_Conversion;

package body ADXL345 is

   package Addr is
      DEVID          : constant UInt8 := 16#00#;
      THRESH_TAP     : constant UInt8 := 16#1D#;
      OFSX           : constant UInt8 := 16#1E#;
      OFSY           : constant UInt8 := 16#1F#;
      OFSZ           : constant UInt8 := 16#20#;
      DUR            : constant UInt8 := 16#21#;
      Latent         : constant UInt8 := 16#22#;
      Window         : constant UInt8 := 16#23#;
      THRESH_ACT     : constant UInt8 := 16#24#;
      THRESH_INACT   : constant UInt8 := 16#25#;
      TIME_INACT     : constant UInt8 := 16#26#;
      ACT_INACT_CTL  : constant UInt8 := 16#27#;
      THRESH_FF      : constant UInt8 := 16#28#;
      TIME_FF        : constant UInt8 := 16#29#;
      TAP_AXES       : constant UInt8 := 16#2A#;
      ACT_TAP_STATUS : constant UInt8 := 16#2B#;
      BW_RATE        : constant UInt8 := 16#2C#;
      POWER_CTL      : constant UInt8 := 16#2D#;
      INT_ENABLE     : constant UInt8 := 16#2E#;
      INT_MAP        : constant UInt8 := 16#2F#;
      INT_SOURCE     : constant UInt8 := 16#30#;
      DATA_FORMAT    : constant UInt8 := 16#31#;
      DATAX0         : constant UInt8 := 16#32#;
      DATAX1         : constant UInt8 := 16#33#;
      DATAY0         : constant UInt8 := 16#34#;
      DATAY1         : constant UInt8 := 16#35#;
      DATAZ0         : constant UInt8 := 16#36#;
      DATAZ1         : constant UInt8 := 16#37#;
      FIFO_CTL       : constant UInt8 := 16#38#;
      FIFO_STATUS    : constant UInt8 := 16#39#;
   end Addr;

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

   generic
      Reg : UInt8;
      type Register_Type is private;
   package Register is
      function Get return Register_Type;
      procedure Set (Val : Register_Type);
   end Register;

   package body Register is
      subtype Register_Array is UInt8_Array (1 .. Register_Type'Size / 8);

      function Get
         return Register_Type
      is
         function Convert is new Ada.Unchecked_Conversion (Register_Array, Register_Type);
         Data : Register_Array;
      begin
         Read (Reg, Data);
         return Convert (Data);
      end Get;

      procedure Set
         (Val : Register_Type)
      is
         function Convert is new Ada.Unchecked_Conversion (Register_Type, Register_Array);
         Data : constant Register_Array := Convert (Val);
      begin
         Write (Reg, Data);
      end Set;
   end Register;

   procedure Configure
      (Watermark : UInt5)
   is
      package POWER_CTL is new Register (Addr.POWER_CTL, POWER_CTL_Register);
      package DATA_FORMAT is new Register (Addr.DATA_FORMAT, DATA_FORMAT_Register);
      package FIFO_CTL is new Register (Addr.FIFO_CTL, FIFO_CTL_Register);
   begin
      DATA_FORMAT.Set (DATA_FORMAT_Register'
         (SELF_TEST  => False,
          SPI        => False, --  4 wire mode
          INT_INVERT => False,
          FULL_RES   => False,
          Justify    => False,
          G_Range    => 0));
      FIFO_CTL.Set (FIFO_CTL_Register'
         (FIFO_MODE  => Stream,
          Trigger    => False,
          Samples    => Watermark));
      POWER_CTL.Set (POWER_CTL_Register'
         (Link       => False,
          AUTO_SLEEP => False,
          Measure    => True,
          Sleep      => False,
          Wakeup     => Wake_1_Hz));
   end Configure;

   procedure Read
      (M : out Measurement)
   is
      package DATA is new Register (Addr.DATAX0, Measurement);
   begin
      M := DATA.Get;
   end Read;

end ADXL345;
