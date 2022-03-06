with NRF24L01_Registers; use NRF24L01_Registers;
with HAL; use HAL;

package body NRF24L01 is
   procedure Initialize
      (This   : in out Device;
       P      : Pins;
       Delays : not null HAL.Time.Any_Delays)
   is
   begin
      This.P := P;
      This.Delays := Delays;
      This.Reset;

      CONFIG.Write (This.P, CONFIG_Register'(others => <>), Verify => True);

      SETUP_RETR.Write (This.P, SETUP_RETR_Register'
         (ARD => 15,   --  Auto Retransmit Delay (1_500 microseconds between retransmits)
          ARC => 15)); --  Auto Retransmit Count (up to 15 times)
      RF_SETUP.Write (This.P, RF_SETUP_Register'
         (CONT_WAVE  => False,
          RF_DR_LOW  => False,
          PLL_LOCK   => False,
          RF_DR_HIGH => False,   --  1 Mbps
          RF_PWR     => 2#11#)); --  Max transmit power
      --  Disable dynamic payloads
      DYNPD.Write (This.P, DYNPD_Register'(others => False));
      --  Enable auto-ack on all pipes
      EN_AA.Write (This.P, EN_AA_Register'(others => True));
      --  Open RX pipes 0 and 1
      EN_RXADDR.Write (This.P, EN_RXADDR_Register'
         (P0     => True,
          P1     => True,
          others => False));
      --  Set static payload size to max (32 bytes) for all pipes
      RX_PW_P0.Write (This.P, 32);
      RX_PW_P1.Write (This.P, 32);
      RX_PW_P2.Write (This.P, 32);
      RX_PW_P3.Write (This.P, 32);
      RX_PW_P4.Write (This.P, 32);
      RX_PW_P5.Write (This.P, 32);
      --  5 byte addressing
      SETUP_AW.Write (This.P, AW_5_Bytes);
      --  Default to channel 76, which won't bleed into other bands.
      RF_CH.Write (This.P, 76);
      --  Clear status flags
      STATUS.Write (This.P, STATUS_Register'
         (RX_DR  => True,
          TX_DS  => True,
          MAX_RT => True,
          others => <>));

      NRF24L01_IO.Flush_RX (This.P);
      NRF24L01_IO.Flush_TX (This.P);

      declare
         C : CONFIG_Register :=
            (PRIM_RX => PRX,
             others  => <>);
      begin
         C.PRIM_RX := PRX;
         CONFIG.Write (This.P, C, Verify => True);
         C.EN_CRC := True;
         C.CRCO := True;
         CONFIG.Write (This.P, C, Verify => True);
         C.PWR_UP := True;
         CONFIG.Write (This.P, C);
         This.Delays.Delay_Milliseconds (5); --  Tpd2stdby
         if CONFIG.Read (This.P) /= C then
            raise NRF_Error with "Power up failed";
         end if;
      end;
   end Initialize;

   procedure Reset
      (This : in out Device)
   is
      use HAL.Time;
      Tpor : constant := 100;
      Thce : constant := 1;
      --  Thce is actually 10 microseconds, but HAL.Time doesn't do that.
   begin
      This.P.CS.Set;
      This.P.CE.Clear;
      This.Delays.Delay_Milliseconds (Tpor);
      This.P.CE.Set;
      This.Delays.Delay_Milliseconds (Thce);
   end Reset;

end NRF24L01;
