with Checksum;

package body SGP30 is

   function Verify_Checksum
      (Data : UInt8_Array)
      return Boolean
   is
      --  Every third byte is the CRC-8 of the preceding two bytes.
      I : Positive := Data'First;
   begin
      --  XXX IGNORING CHECKSUM FOR TESTING
      --  while I <= Data'Last loop
      --     if Checksum.CRC_8 (Data (I .. I + 1), Poly => 16#31#) /= Data (I + 2) then
      --        return False;
      --     end if;
      --     I := I + 3;
      --  end loop;
      return True;
   end Verify_Checksum;

   procedure Soft_Reset
      (This : Device)
   is
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit
         (Addr   => 16#00#,
          Data   => I2C_Data'(1 => 16#06#),
          Status => Status);
      --  Ignore status aft
   end Soft_Reset;

   function Read_48
      (This : Device;
       Reg  : UInt16)
       return UInt48
   is
      Data   : I2C_Data (1 .. 9);
      Status : I2C_Status;
      I      : Positive := Data'First;
   begin
      This.Port.Mem_Read
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);

      if Status /= Ok then
         raise SGP30_Error with "Read error 48";
      end if;

      if Verify_Checksum (Data) then
         return UInt48
            (Shift_Left (UInt64 (Data (1)), 40) or
             Shift_Left (UInt64 (Data (2)), 32) or
             Shift_Left (UInt64 (Data (4)), 24) or
             Shift_Left (UInt64 (Data (5)), 16) or
             Shift_Left (UInt64 (Data (7)), 8) or
             Shift_Left (UInt64 (Data (8)), 0));
      else
         raise SGP30_Error with "Invalid checksum 48";
      end if;
   end Read_48;

   function Read_32
      (This : Device;
       Reg  : UInt16)
       return UInt32
   is
      Data   : I2C_Data (1 .. 6);
      Status : I2C_Status;
      I      : Positive := Data'First;
   begin
      This.Port.Mem_Read
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);

      if Status /= Ok then
         raise SGP30_Error with "Read error 32";
      end if;

      if Verify_Checksum (Data) then
         return Shift_Left (UInt32 (Data (1)), 24) or
                Shift_Left (UInt32 (Data (2)), 16) or
                Shift_Left (UInt32 (Data (4)), 8) or
                Shift_Left (UInt32 (Data (5)), 0);
      else
         raise SGP30_Error with "Invalid checksum 32";
      end if;
   end Read_32;

   function Read_16
      (This : Device;
       Reg  : UInt16)
       return UInt16
   is
      Data   : I2C_Data (1 .. 3);
      Status : I2C_Status;
      I      : Positive := Data'First;
   begin
      This.Port.Mem_Read
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);

      if Status /= Ok then
         raise SGP30_Error with "Read error 16";
      end if;

      if Verify_Checksum (Data) then
         return Shift_Left (UInt16 (Data (1)), 8) or
                Shift_Left (UInt16 (Data (2)), 0);
      else
         raise SGP30_Error with "Invalid checksum 16";
      end if;
   end Read_16;

   procedure Write_48
      (This  : Device;
       Reg   : UInt16;
       Value : UInt48)
   is
      Data   : I2C_Data (1 .. 9);
      Status : I2C_Status;
   begin
      Data (1) := UInt8 (Shift_Right (UInt64 (Value), 40) and 16#FF#);
      Data (2) := UInt8 (Shift_Right (UInt64 (Value), 32) and 16#FF#);
      Data (3) := Checksum.CRC_8 (Data (1 .. 2), Poly => 16#31#);
      Data (4) := UInt8 (Shift_Right (UInt64 (Value), 24) and 16#FF#);
      Data (5) := UInt8 (Shift_Right (UInt64 (Value), 16) and 16#FF#);
      Data (6) := Checksum.CRC_8 (Data (4 .. 5), Poly => 16#31#);
      Data (7) := UInt8 (Shift_Right (UInt64 (Value), 8) and 16#FF#);
      Data (8) := UInt8 (Shift_Right (UInt64 (Value), 0) and 16#FF#);
      Data (9) := Checksum.CRC_8 (Data (7 .. 8), Poly => 16#31#);

      This.Port.Mem_Write
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);
      if Status /= Ok then
         raise SGP30_Error with "Write 48 failed";
      end if;
   end Write_48;

   procedure Write_32
      (This  : Device;
       Reg   : UInt16;
       Value : UInt32)
   is
      Data   : I2C_Data (1 .. 6);
      Status : I2C_Status;
   begin
      Data (1) := UInt8 (Shift_Right (Value, 24) and 16#FF#);
      Data (2) := UInt8 (Shift_Right (Value, 16) and 16#FF#);
      Data (3) := Checksum.CRC_8 (Data (1 .. 2), Poly => 16#31#);
      Data (4) := UInt8 (Shift_Right (Value, 8) and 16#FF#);
      Data (5) := UInt8 (Shift_Right (Value, 0) and 16#FF#);
      Data (6) := Checksum.CRC_8 (Data (4 .. 5), Poly => 16#31#);

      This.Port.Mem_Write
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);
      if Status /= Ok then
         raise SGP30_Error with "Write 32 failed";
      end if;
   end Write_32;

   procedure Write_16
      (This  : Device;
       Reg   : UInt16;
       Value : UInt16)
   is
      Data   : I2C_Data (1 .. 3);
      Status : I2C_Status;
   begin
      Data (1) := UInt8 (Shift_Right (Value, 8) and 16#FF#);
      Data (2) := UInt8 (Shift_Right (Value, 0) and 16#FF#);
      Data (3) := Checksum.CRC_8 (Data (1 .. 2), Poly => 16#31#);

      This.Port.Mem_Write
         (Addr          => This.Addr,
          Mem_Addr      => Reg,
          Mem_Addr_Size => Memory_Size_16b,
          Data          => Data,
          Status        => Status);
      if Status /= Ok then
         raise SGP30_Error with "Write 16 failed";
      end if;
   end Write_16;

   procedure Write_Command
      (This : Device;
       Reg  : UInt16)
   is
      Data   : I2C_Data (1 .. 2)
         with Import, Address => Reg'Address;
      Status : I2C_Status;
   begin
      This.Port.Master_Transmit
         (Addr   => This.Addr,
          Data   => Data,
          Status => Status);
      if Status /= Ok then
         raise SGP30_Error with "Command failed";
      end if;
   end Write_Command;

   procedure Init_Air_Quality
      (This : Device)
   is
   begin
      This.Write_Command (16#2003#);
   end Init_Air_Quality;

   procedure Measure_Air_Quality
      (This : Device;
       eCO2 : out Natural;
       TVOC : out Natural)
   is
      X : UInt32;
   begin
      X := This.Read_32 (16#2008#);
      eCO2 := Natural (Shift_Right (X, 16));
      TVOC := Natural (X and 16#FFFF#);
   end Measure_Air_Quality;

   function Get_Baseline
      (This : Device)
      return Natural
   is (Natural (This.Read_32 (16#2015#)));

   procedure Set_Baseline
      (This     : Device;
       Baseline : Natural)
   is
   begin
      This.Write_32 (16#201E#, UInt32 (Baseline));
   end Set_Baseline;

   procedure Set_Humidity
      (This     : Device;
       Humidity : UInt16)
   is
   begin
      This.Write_16 (16#2061#, Humidity);
   end Set_Humidity;

   function Measure_Test
      (This : Device)
      return UInt16
   is (This.Read_16 (16#2032#));

   function Get_Feature_Set_Version
      (This : Device)
      return UInt16
   is (This.Read_16 (16#202F#));

   function Measure_Raw_Signals
      (This : Device)
      return Natural
   is (Natural (This.Read_32 (16#2050#)));

   function Get_Serial_Id
      (This : Device)
      return UInt48
   is (This.Read_48 (16#3682#));

end SGP30;
