with Ada.Unchecked_Conversion;

package body ADXL345 is
   generic
      Reg : Register;
      type Register_Type is private;
   procedure Set
      (Value : Register_Type);

   generic
      Reg : Register;
      type Register_Type is private;
   function Get
      return Register_Type;

   procedure Set
      (Value : Register_Type)
   is
      subtype Data_Array is UInt8_Array (1 .. Register_Type'Size / 8);
      function Convert is new Ada.Unchecked_Conversion (Register_Type, Data_Array);
   begin
      Write_Register (Register'Enum_Rep (Reg), Convert (Value));
   end Set;

   function Get
      return Register_Type
   is
      subtype Data_Array is UInt8_Array (1 .. Register_Type'Size / 8);
      function Convert is new Ada.Unchecked_Conversion (Data_Array, Register_Type);
      Data : Data_Array;
   begin
      Read_Register (Register'Enum_Rep (Reg), Data);
      return Convert (Data);
   end Get;

   procedure Initialize is
      procedure Set_INT_ENABLE is new Set (INT_ENABLE, INT_ENABLE_Register);
      procedure Set_INT_MAP is new Set (INT_MAP, INT_MAP_Register);
      procedure Set_DATA_FORMAT is new Set (DATA_FORMAT, DATA_FORMAT_Register);
      procedure Set_FIFO_CTL is new Set (FIFO_CTL, FIFO_CTL_Register);
      procedure Set_POWER_CTL is new Set (POWER_CTL, POWER_CTL_Register);
   begin
      Set_POWER_CTL
         ((Link         => False,
           AUTO_SLEEP   => False,
           Measure      => False,
           Sleep        => False,
           Wakeup       => Wake_1_Hz));

      Set_INT_ENABLE
         ((DATA_READY   => False,
           SINGLE_TAP   => False,
           DOUBLE_TAP   => False,
           Activity     => False,
           Inactivity   => False,
           FREE_FALL    => False,
           Watermark    => True,
           Overrun      => False));

      Set_INT_MAP
         ((DATA_READY   => False,
           SINGLE_TAP   => False,
           DOUBLE_TAP   => False,
           Activity     => False,
           Inactivity   => False,
           FREE_FALL    => False,
           Watermark    => False,
           Overrun      => False));

      Set_DATA_FORMAT
         ((SELF_TEST    => False,
           SPI          => False,
           INT_INVERT   => False,
           FULL_RES     => True,
           Justify      => False,
           G_Range      => 2#00#));

      Set_FIFO_CTL
         ((FIFO_MODE => Stream,
           Trigger   => False,
           Samples   => 2));

      Set_POWER_CTL
         ((Link         => False,
           AUTO_SLEEP   => False,
           Measure      => True,
           Sleep        => False,
           Wakeup       => Wake_1_Hz));
   end Initialize;

   procedure Read_Raw
      (X, Y, Z : out Int16)
   is
      type Data_Array is array (1 .. 3) of Int16
         with Component_Size => 16;
      function Get_Data is new Get (DATAX0, Data_Array);
      Data : Data_Array;
   begin
      Data := Get_Data;
      X := Data (1);
      Y := Data (2);
      Z := Data (3);
   end Read_Raw;

   procedure Read
      (X, Y, Z : out Float)
   is
      G_Range : constant Float := 0.004;
      RX, RY, RZ : Int16;
   begin
      Read_Raw (RX, RY, RZ);
      X := Float (RX) * G_Range;
      Y := Float (RY) * G_Range;
      Z := Float (RZ) * G_Range;
   end Read;

   function Status
      return INT_SOURCE_Register
   is
      function Get_INT_SOURCE is new Get (INT_SOURCE, INT_SOURCE_Register);
   begin
      return Get_INT_SOURCE;
   end Status;

   procedure Set_Offset
      (X, Y, Z : Int16)
   is
      type Data_Array is array (1 .. 3) of Int16
         with Component_Size => 16;
      procedure Set_OFS is new Set (OFSX, Data_Array);
   begin
      Set_OFS ((X, Y, Z));
   end Set_Offset;
end ADXL345;
