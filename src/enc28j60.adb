--
--  Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: BSD-3-Clause
--
pragma Warnings (Off, "* is not referenced");

package body ENC28J60 is
   --  Inspired by https://github.com/bprayudha/avr-enc28j60

   ADDR_MASK : constant UInt8 := 16#1F#;
   BANK_MASK : constant UInt8 := 16#60#;

   --  All Banks Registers
   EIE   : constant UInt8 := 16#1B#;
   EIR   : constant UInt8 := 16#1C#;
   ESTAT : constant UInt8 := 16#1D#;
   ECON2 : constant UInt8 := 16#1E#;
   ECON1 : constant UInt8 := 16#1F#;

   --  Bank 0 Registers
   ERDPTL   : constant UInt8 := 16#00#;
   ERDPTH   : constant UInt8 := 16#01#;
   EWRPTL   : constant UInt8 := 16#02#;
   EWRPTH   : constant UInt8 := 16#03#;
   ETXSTL   : constant UInt8 := 16#04#;
   ETXSTH   : constant UInt8 := 16#05#;
   ETXNDL   : constant UInt8 := 16#06#;
   ETXNDH   : constant UInt8 := 16#07#;
   ERXSTL   : constant UInt8 := 16#08#;
   ERXSTH   : constant UInt8 := 16#09#;
   ERXNDL   : constant UInt8 := 16#0A#;
   ERXNDH   : constant UInt8 := 16#0B#;
   ERXRDPTL : constant UInt8 := 16#0C#;
   ERXRDPTH : constant UInt8 := 16#0D#;
   ERXWRPTL : constant UInt8 := 16#0E#;
   ERXWRPTH : constant UInt8 := 16#0F#;
   EDMASTL  : constant UInt8 := 16#10#;
   EDMASTH  : constant UInt8 := 16#11#;
   EDMANDL  : constant UInt8 := 16#12#;
   EDMANDH  : constant UInt8 := 16#13#;
   EDMADSTL : constant UInt8 := 16#14#;
   EDMADSTH : constant UInt8 := 16#15#;
   EDMACSL  : constant UInt8 := 16#16#;
   EDMACSH  : constant UInt8 := 16#17#;

   --  Bank 1 Registers
   EHT0     : constant UInt8 := 16#20#;
   EHT1     : constant UInt8 := 16#21#;
   EHT2     : constant UInt8 := 16#22#;
   EHT3     : constant UInt8 := 16#23#;
   EHT4     : constant UInt8 := 16#24#;
   EHT5     : constant UInt8 := 16#25#;
   EHT6     : constant UInt8 := 16#26#;
   EHT7     : constant UInt8 := 16#27#;
   EPMM0    : constant UInt8 := 16#28#;
   EPMM1    : constant UInt8 := 16#29#;
   EPMM2    : constant UInt8 := 16#2A#;
   EPMM3    : constant UInt8 := 16#2B#;
   EPMM4    : constant UInt8 := 16#2C#;
   EPMM5    : constant UInt8 := 16#2D#;
   EPMM6    : constant UInt8 := 16#2E#;
   EPMM7    : constant UInt8 := 16#2F#;
   EPMCSL   : constant UInt8 := 16#30#;
   EPMCSH   : constant UInt8 := 16#31#;
   EPMOL    : constant UInt8 := 16#34#;
   EPMOH    : constant UInt8 := 16#35#;
   EWOLIE   : constant UInt8 := 16#36#;
   EWOLIR   : constant UInt8 := 16#37#;
   ERXFCON  : constant UInt8 := 16#38#;
   EPKTCNT  : constant UInt8 := 16#39#;

   --  Bank 2 Register
   MACON1   : constant UInt8 := 16#C0#;
   MACON2   : constant UInt8 := 16#C1#;
   MACON3   : constant UInt8 := 16#C2#;
   MACON4   : constant UInt8 := 16#C3#;
   MABBIPG  : constant UInt8 := 16#C4#;
   MAIPGL   : constant UInt8 := 16#C6#;
   MAIPGH   : constant UInt8 := 16#C7#;
   MACLCON1 : constant UInt8 := 16#C8#;
   MACLCON2 : constant UInt8 := 16#C9#;
   MAMXFLL  : constant UInt8 := 16#CA#;
   MAMXFLH  : constant UInt8 := 16#CB#;
   MAPHSUP  : constant UInt8 := 16#CD#;
   MICON    : constant UInt8 := 16#D1#;
   MICMD    : constant UInt8 := 16#D2#;
   MIREGADR : constant UInt8 := 16#D4#;
   MIWRL    : constant UInt8 := 16#D6#;
   MIWRH    : constant UInt8 := 16#D7#;
   MIRDL    : constant UInt8 := 16#D8#;
   MIRDH    : constant UInt8 := 16#D9#;

   --  Bank 3 Registers
   MAADR1   : constant UInt8 := 16#E0#;
   MAADR0   : constant UInt8 := 16#E1#;
   MAADR3   : constant UInt8 := 16#E2#;
   MAADR2   : constant UInt8 := 16#E3#;
   MAADR5   : constant UInt8 := 16#E4#;
   MAADR4   : constant UInt8 := 16#E5#;
   EBSTSD   : constant UInt8 := 16#66#;
   EBSTCON  : constant UInt8 := 16#67#;
   EBSTCSL  : constant UInt8 := 16#68#;
   EBSTCSH  : constant UInt8 := 16#69#;
   MISTAT   : constant UInt8 := 16#EA#;
   EREVID   : constant UInt8 := 16#72#;
   ECOCON   : constant UInt8 := 16#75#;
   EFLOCON  : constant UInt8 := 16#77#;
   EPAUSL   : constant UInt8 := 16#78#;
   EPAUSH   : constant UInt8 := 16#79#;

   --  PHY Registers
   PHCON1    : constant UInt8 := 16#00#;
   PHSTAT1   : constant UInt8 := 16#01#;
   PHHID1    : constant UInt8 := 16#02#;
   PHHID2    : constant UInt8 := 16#03#;
   PHCON2    : constant UInt8 := 16#10#;
   PHSTAT2   : constant UInt8 := 16#11#;
   PHIE      : constant UInt8 := 16#12#;
   PHIR      : constant UInt8 := 16#13#;
   PHLCON    : constant UInt8 := 16#14#;

   --  ERXFCON bit definitions
   UCEN      : constant UInt8 := 16#80#;
   ANDOR     : constant UInt8 := 16#40#;
   CRCEN     : constant UInt8 := 16#20#;
   PMEN      : constant UInt8 := 16#10#;
   MPEN      : constant UInt8 := 16#08#;
   HTEN      : constant UInt8 := 16#04#;
   MCEN      : constant UInt8 := 16#02#;
   BCEN      : constant UInt8 := 16#01#;

   --  EIE bit definitions
   INTIE     : constant UInt8 := 16#80#;
   PKTIE     : constant UInt8 := 16#40#;
   DMAIE     : constant UInt8 := 16#20#;
   LINKIE    : constant UInt8 := 16#10#;
   TXIE      : constant UInt8 := 16#08#;
   WOLIE     : constant UInt8 := 16#04#;
   TXERIE    : constant UInt8 := 16#02#;
   RXERIE    : constant UInt8 := 16#01#;

   --  EIR bit definitions
   PKTIF     : constant UInt8 := 16#40#;
   DMAIF     : constant UInt8 := 16#20#;
   LINKIF    : constant UInt8 := 16#10#;
   TXIF      : constant UInt8 := 16#08#;
   WOLIF     : constant UInt8 := 16#04#;
   TXERIF    : constant UInt8 := 16#02#;
   RXERIF    : constant UInt8 := 16#01#;

   --  ESTAT bit definitions
   INT       : constant UInt8 := 16#80#;
   LATECOL   : constant UInt8 := 16#10#;
   RXBUSY    : constant UInt8 := 16#04#;
   TXABRT    : constant UInt8 := 16#02#;
   CLKRDY    : constant UInt8 := 16#01#;

   --  ECON2 bit definitions
   AUTOINC   : constant UInt8 := 16#80#;
   PKTDEC    : constant UInt8 := 16#40#;
   PWRSV     : constant UInt8 := 16#20#;
   VRPS      : constant UInt8 := 16#08#;

   --  ECON1 bit definitions
   TXRST     : constant UInt8 := 16#80#;
   RXRST     : constant UInt8 := 16#40#;
   DMAST     : constant UInt8 := 16#20#;
   CSUMEN    : constant UInt8 := 16#10#;
   TXRTS     : constant UInt8 := 16#08#;
   RXEN      : constant UInt8 := 16#04#;
   BSEL1     : constant UInt8 := 16#02#;
   BSEL0     : constant UInt8 := 16#01#;

   --  MACON1 bit definitions
   LOOPBK    : constant UInt8 := 16#10#;
   TXPAUS    : constant UInt8 := 16#08#;
   RXPAUS    : constant UInt8 := 16#04#;
   PASSALL   : constant UInt8 := 16#02#;
   MARXEN    : constant UInt8 := 16#01#;

   --  MACON2 bit definitions
   MARST     : constant UInt8 := 16#80#;
   RNDRST    : constant UInt8 := 16#40#;
   MARXRST   : constant UInt8 := 16#08#;
   RFUNRST   : constant UInt8 := 16#04#;
   MATXRST   : constant UInt8 := 16#02#;
   TFUNRST   : constant UInt8 := 16#01#;

   --  MACON3 bit definitions
   PADCFG2   : constant UInt8 := 16#80#;
   PADCFG1   : constant UInt8 := 16#40#;
   PADCFG0   : constant UInt8 := 16#20#;
   TXCRCEN   : constant UInt8 := 16#10#;
   PHDRLEN   : constant UInt8 := 16#08#;
   HFRMLEN   : constant UInt8 := 16#04#;
   FRMLNEN   : constant UInt8 := 16#02#;
   FULDPX    : constant UInt8 := 16#01#;

   --  MICMD bit definitions
   MIISCAN   : constant UInt8 := 16#02#;
   MIIRD     : constant UInt8 := 16#01#;

   --  MISTAT bit definitions
   NVALID    : constant UInt8 := 16#04#;
   SCAN      : constant UInt8 := 16#02#;
   BUSY      : constant UInt8 := 16#01#;

   --  PHCON1 bit definitions
   PRST      : constant UInt16 := 16#8000#;
   PLOOPBK   : constant UInt16 := 16#4000#;
   PPWRSV    : constant UInt16 := 16#0800#;
   PDPXMD    : constant UInt16 := 16#0100#;

   --  PHSTAT1 bit definitions
   PFDPX     : constant UInt16 := 16#1000#;
   PHDPX     : constant UInt16 := 16#0800#;
   LLSTAT    : constant UInt16 := 16#0004#;
   JBSTAT    : constant UInt16 := 16#0002#;

   --  PHCON2 bit definitions
   FRCLINK   : constant UInt16 := 16#4000#;
   TXDIS     : constant UInt16 := 16#2000#;
   JABBER    : constant UInt16 := 16#0400#;
   HDLDIS    : constant UInt16 := 16#0100#;

   --  Packet Control bit Definitions
   PHUGEEN   : constant UInt8 := 16#08#;
   PPADEN    : constant UInt8 := 16#04#;
   PCRCEN    : constant UInt8 := 16#02#;
   POVERRIDE : constant UInt8 := 16#01#;

   --  SPI ops
   RCR   : constant UInt8 := 16#00#;   --  Read Control Register
   RBM   : constant UInt8 := 16#3A#;   --  Read Buffer Memory
   WCR   : constant UInt8 := 16#40#;   --  Write Control Register
   WBM   : constant UInt8 := 16#7A#;   --  Write Buffer Memory
   BFS   : constant UInt8 := 16#80#;   --  Bit Field Set
   BFC   : constant UInt8 := 16#A0#;   --  Bit Field Clear
   SC    : constant UInt8 := 16#FF#;   --  Soft Reset

   RXSTART_INIT   : constant UInt16 := 16#0000#;
   RXSTOP_INIT    : constant UInt16 := 16#1FFF# - 16#0600# - 1;
   TXSTART_INIT   : constant UInt16 := 16#1FFF# - 16#0600#;
   TXSTOP_INIT    : constant UInt16 := 16#1FFF#;
   MAX_FRAMELEN   : constant := 1500;

   procedure SPI_Write
      (This : Device;
       Data : UInt8)
   is
      D : UInt8 := Data;
   begin
      This.SPI_Transfer (D);
      pragma Unreferenced (D);
   end SPI_Write;

   procedure Read_Op
      (This : Device;
       Op   : UInt8;
       Addr : UInt8;
       Data : out UInt8)
   is
   begin
      This.CS (False);

      Data := Op or (Addr and ADDR_MASK);
      SPI_Write (This, Data);

      Data := 0;
      This.SPI_Transfer (Data);

      if (Addr and 16#80#) /= 0 then
         Data := 0;
         This.SPI_Transfer (Data);
      end if;

      This.CS (True);
   end Read_Op;

   procedure Write_Op
      (This : Device;
       Op   : UInt8;
       Addr : UInt8;
       Data : UInt8)
   is
      D : UInt8;
   begin
      This.CS (False);
      D := Op or (Addr and ADDR_MASK);
      SPI_Write (This, D);
      D := Data;
      SPI_Write (This, D);
      This.CS (True);
   end Write_Op;

   procedure Set_Bank
      (This : in out Device;
       Addr : UInt8)
   is
   begin
      if (Addr and BANK_MASK) /= This.Bank then
         Write_Op (This, BFC, ECON1, BSEL1 or BSEL0);
         Write_Op (This, BFS, ECON1, Shift_Right (Addr and BANK_MASK, 5));
         This.Bank := Addr and BANK_MASK;
      end if;
   end Set_Bank;

   procedure Write
      (This : in out Device;
       Addr : UInt8;
       Data : UInt8)
   is
   begin
      Set_Bank (This, Addr);
      Write_Op (This, WCR, Addr, Data);
   end Write;

   procedure Write_16
      (This : in out Device;
       Addr : UInt8;
       Data : UInt16)
   is
   begin
      Write (This, Addr, UInt8 (Data and 16#FF#));
      Write (This, Addr + 1, UInt8 (Shift_Right (Data, 8)));
   end Write_16;

   procedure Read
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt8)
   is
   begin
      Set_Bank (This, Addr);
      Read_Op (This, RCR, Addr, Data);
   end Read;

   procedure Read_16
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt16)
   is
      X : UInt8;
   begin
      Read (This, Addr, X);
      Data := UInt16 (X);
      Read (This, Addr + 1, X);
      Data := Data or Shift_Left (UInt16 (X), 8);
   end Read_16;

   procedure Phy_Write
      (This : in out Device;
       Addr : UInt8;
       Data : UInt16)
   is
      Status : UInt8;
   begin
      Write (This, MIREGADR, Addr);
      Write_16 (This, MIWRL, Data);
      loop
         Read (This, MISTAT, Status);
         exit when (Status and BUSY) = 0;
      end loop;
   end Phy_Write;

   procedure Phy_Read
      (This : in out Device;
       Addr : UInt8;
       Data : out UInt16)
   is
      Status : UInt8;
   begin
      Write (This, MIREGADR, Addr);
      Write (This, MICMD, MIIRD);
      loop
         Read (This, MISTAT, Status);
         exit when (Status and BUSY) = 0;
      end loop;
      Write (This, MICMD, 16#00#);
      Read_16 (This, MIRDH, Data);
   end Phy_Read;

   procedure Write_Buffer
      (This : Device;
       Data : UInt8_Array)
   is
   begin
      This.CS (False);
      SPI_Write (This, WBM);
      for D of Data loop
         SPI_Write (This, D);
      end loop;
      This.CS (True);
   end Write_Buffer;

   procedure Read_Buffer
      (This : Device;
       Data : out UInt8_Array)
   is
   begin
      Data := (others => 0);
      This.CS (False);
      SPI_Write (This, RBM);
      for I in Data'Range loop
         This.SPI_Transfer (Data (I));
      end loop;
      This.CS (True);
   end Read_Buffer;

   procedure Initialize
      (This : in out Device;
       MAC  : MAC_Address)
   is
   begin
      Write_Op (This, SC, 0, SC);
      This.Next_Packet_Ptr := RXSTART_INIT;
      Write_16 (This, ERXSTL, RXSTART_INIT);
      Write_16 (This, ERXRDPTL, RXSTART_INIT);
      Write_16 (This, ERXNDL, RXSTOP_INIT);
      Write_16 (This, ETXSTL, TXSTART_INIT);
      Write_16 (This, ETXNDL, TXSTOP_INIT);
      Write (This, ERXFCON, UCEN or CRCEN or PMEN);
      Write (This, EPMM0, 16#3F#);
      Write (This, EPMM1, 16#30#);
      Write_16 (This, EPMCSL, 16#F7F9#);
      Write (This, MACON1, MARXEN or TXPAUS or RXPAUS);
      Write_Op (This, BFS, MACON3, PADCFG0 or TXCRCEN or FRMLNEN);
      Write_16 (This, MAIPGL, 16#0C12#);
      Write (This, MABBIPG, 16#12#);
      Write_16 (This, MAMXFLL, MAX_FRAMELEN);
      Write (This, MAADR5, MAC (1));
      Write (This, MAADR4, MAC (2));
      Write (This, MAADR3, MAC (3));
      Write (This, MAADR2, MAC (4));
      Write (This, MAADR1, MAC (5));
      Write (This, MAADR0, MAC (6));
      Phy_Write (This, PHCON2, HDLDIS);
      Set_Bank (This, ECON1);
      Write_Op (This, BFS, EIE, INTIE or PKTIE);
      Write_Op (This, BFS, ECON1, RXEN);
      This.Init_Done := True;
   end Initialize;

   procedure Packet_Transmit
      (This : in out Device;
       Data : Packet_Data;
       Last : Natural)
   is
      Status : UInt8;
   begin
      loop
         Read_Op (This, RCR, ECON1, Status);
         exit when (Status and TXRTS) = 0;
         Read (This, EIR, Status);
         if (Status and TXERIF) /= 0 then
            Write_Op (This, BFS, ECON1, TXRST);
            Write_Op (This, BFC, ECON1, TXRST);
         end if;
      end loop;

      Write_16 (This, EWRPTL, TXSTART_INIT);
      Write_16 (This, ETXNDL, TXSTART_INIT + UInt16 (Last));
      Write_Op (This, WBM, 0, 0);
      Write_Buffer (This, Data (1 .. Last));
      Write_Op (This, BFS, ECON1, TXRTS);
   end Packet_Transmit;

   procedure Packet_Receive
      (This : in out Device;
       Data : out Packet_Data;
       Last : out Natural)
   is
      RXStat   : UInt16;
      Len      : UInt16;
      X        : UInt8;
   begin
      Data := (others => 0);

      Read (This, EPKTCNT, X);
      if X = 0 then
         Last := 0;
         return;
      end if;

      Write_16 (This, ERDPTL, This.Next_Packet_Ptr);
      Read_Op (This, RBM, 0, X);
      This.Next_Packet_Ptr := UInt16 (X);
      Read_Op (This, RBM, 0, X);
      This.Next_Packet_Ptr := This.Next_Packet_Ptr or Shift_Left (UInt16 (X), 8);

      Read_Op (This, RBM, 0, X);
      Len := UInt16 (X);
      Read_Op (This, RBM, 0, X);
      Len := Len or Shift_Left (UInt16 (X), 8);
      Len := Len - 4;
      Last := Natural (Len);

      Read_Op (This, RBM, 0, X);
      RXStat := UInt16 (X);
      Read_Op (This, RBM, 0, X);
      RXStat := RXStat or Shift_Left (UInt16 (X), 8);

      if Last > Data'Length then
         Last := Data'Length;
      end if;

      if (RXStat and 16#80#) = 0 then
         Last := 0;
      else
         Read_Buffer (This, Data (1 .. Last));
      end if;

      Write_16 (This, ERXRDPTL, This.Next_Packet_Ptr);

      --  if (This.Next_Packet_Ptr - 1) not in RXSTART_INIT .. RXSTOP_INIT then
      --     Write_16 (This, ERXRDPTL, RXSTOP_INIT);
      --  else
      Write_16 (This, ERXRDPTL, This.Next_Packet_Ptr - 1);

      Write_Op (This, BFS, ECON2, PKTDEC);
   end Packet_Receive;

   procedure Get_Link_Status
      (This : in out Device;
       Up   : out Boolean)
   is
      X : UInt16;
   begin
      Phy_Read (This, PHSTAT2, X);
      Up := (X and 4) /= 0;
   end Get_Link_Status;

   procedure Get_Silicon_Revision
      (This : in out Device;
       Rev  : out UInt8)
   is
   begin
      Read (This, EREVID, Rev);
   end Get_Silicon_Revision;

end ENC28J60;
