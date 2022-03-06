with NRF24L01_IO;
with HAL.Time;

package NRF24L01 is

   type Device is tagged private;
   subtype Pins is NRF24L01_IO.Pins;

   NRF_Error : exception;

   procedure Initialize
      (This   : in out Device;
       P      : Pins;
       Delays : not null HAL.Time.Any_Delays);

   procedure Reset
      (This : in out Device);

   type Channel is range 0 .. 125;
   subtype Bits_Per_Second is Integer;
   subtype Data_Rate is Bits_Per_Second
      with Static_Predicate => Data_Rate in 250_000 | 1_000_000 | 2_000_000;

   --  procedure Set_Channel
   --     (This : in out NRF24L01_Device;
   --      Chan : Channel);

   --  procedure Set_Data_Rate
   --     (This : in out Device;
   --      Rate : Data_Rate);

   --  procedure Transmit
   --     (This   : in out Device;
   --      Data   : UInt8_Array;
   --      Status : out Device_State);

   --  procedure Receive
   --     (This   : in out Device;
   --      Data   : out UInt8_Array;
   --      Status : out Device_State);

private

   type Device is tagged record
      P : NRF24L01_IO.Pins;
      Delays : HAL.Time.Any_Delays;
      Plus_Variant : Boolean;
   end record;

end NRF24L01;
