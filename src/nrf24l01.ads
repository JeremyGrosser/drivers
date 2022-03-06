with NRF24L01_IO;
with HAL.Time;
with HAL; use HAL;

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

   type Power_dBm is range -18 .. 0
      with Static_Predicate => Power_dBm in -18 | -12 | -6 | 0;

   procedure Set_Output_Power
      (This : in out Device;
       dBm  : Power_dBm);

   procedure Set_Payload_Length
      (This  : in out Device;
       Bytes : UInt6);

   type Data_Pipe is range 0 .. 5;

   type Address_Bytes is range 3 .. 5;
   type Data_Pipe_Address
      (Width : Address_Bytes)
   is record
      case Width is
         when 3 => Addr_3 : UInt24;
         when 4 => Addr_4 : UInt32;
         when 5 => Addr_5 : UInt40;
      end case;
   end record;

   --  Addresses for pipes P0 and P1 are up to 5 bytes wide.
   --  Pipes P2 .. P5 share P1's prefix and only modify the lowest byte.
   procedure Configure_Receive
      (This : in out Device;
       Pipe : Data_Pipe;
       Addr : Data_Pipe_Address);

   procedure Configure_Transmit
      (This : in out Device;
       Addr : Data_Pipe_Address);

private

   type Device is tagged record
      AW           : Address_Bytes;
      P            : NRF24L01_IO.Pins;
      Delays       : HAL.Time.Any_Delays;
      Plus_Variant : Boolean;
   end record;

end NRF24L01;
