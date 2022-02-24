package body NRF24L01_IO is

   function Is_Error
      (Status : IO_Status)
      return Boolean
   is
      use HAL.SPI;
   begin
      return Status.Bus /= Ok;
   end Is_Error;

   procedure SPI_Transfer
      (P      : Pins;
       Cmd    : HAL.UInt8;
       Data   : in out HAL.SPI.SPI_Data_8b;
       Status : in out IO_Status)
   is
      use HAL.SPI;
      D : SPI_Data_8b (1 .. 1) := (1 => Cmd);
   begin
      P.CS.Clear;
      P.Port.Transmit (D, Status.Bus, Timeout => 0);
      P.Port.Receive (D, Status.Bus, Timeout => 1);
      if Status.Bus /= Ok then
         P.CS.Set;
         return;
      end if;
      Status.Dev := To_STATUS_Register (Data (1));

      --  One byte at a time, we don't know how deep the SPI FIFO is.
      for I in Data'Range loop
         P.Port.Transmit (Data (I .. I), Status.Bus, Timeout => 0);
         P.Port.Receive (Data (I .. I), Status.Bus, Timeout => 1);
         if Status.Bus /= Ok then
            P.CS.Set;
            return;
         end if;
      end loop;
      P.CS.Set;
   end SPI_Transfer;

   procedure Flush_RX
      (P : Pins)
   is
      use HAL.SPI;
      Data   : SPI_Data_8b (1 .. 1) := (1 => 2#1110_0010#);
      Status : SPI_Status;
   begin
      P.CS.Clear;
      P.Port.Transmit (Data, Status, Timeout => 0);
      P.Port.Receive (Data, Status, Timeout => 1);
      P.CS.Set;
   end Flush_RX;

   procedure Flush_TX
      (P : Pins)
   is
      use HAL.SPI;
      Data   : SPI_Data_8b (1 .. 1) := (1 => 2#1110_0001#);
      Status : SPI_Status;
   begin
      P.CS.Clear;
      P.Port.Transmit (Data, Status, Timeout => 0);
      P.Port.Receive (Data, Status, Timeout => 1);
      P.CS.Set;
   end Flush_TX;

   package body Register is
      procedure Read
         (P      : Pins;
          Value  : in out Register_Array;
          Status : in out IO_Status)
      is
      begin
         Value := (others => 0);
         SPI_Transfer (P, Address, Value, Status);
      end Read;

      procedure Read
         (P      : Pins;
          Value  : out Register_Type;
          Status : in out IO_Status)
      is
         V : Register_Array;
      begin
         Read (P, V, Status);
         Value := From_Register_Array (V);
      end Read;

      procedure Write
         (P      : Pins;
          Value  : in out Register_Array;
          Status : in out IO_Status)
      is
         W_REGISTER : constant UInt8 := 2#0010_0000#;
      begin
         SPI_Transfer (P, Address or W_REGISTER, Value, Status);
      end Write;

      procedure Write
         (P      : Pins;
          Value  : Register_Type;
          Status : in out IO_Status)
      is
         V : Register_Array := To_Register_Array (Value);
      begin
         Write (P, V, Status);
      end Write;

      function Read
         (P : Pins)
         return Register_Type
      is
         Status : IO_Status;
         Value  : Register_Type;
      begin
         Read (P, Value, Status);
         if Is_Error (Status) then
            raise IO_Error with "Read " & Address'Image & " failed";
         else
            return Value;
         end if;
      end Read;

      procedure Write
         (P      : Pins;
          Value  : Register_Type;
          Verify : Boolean := False)
      is
         Status : IO_Status;
      begin
         Write (P, Value, Status);
         if Is_Error (Status) then
            raise IO_Error with "Write " & Address'Image & " failed";
         end if;

         if Verify then
            if Read (P) /= Value then
               raise IO_Error with "Write verification failed";
            end if;
         end if;
      end Write;
   end Register;

end NRF24L01_IO;
