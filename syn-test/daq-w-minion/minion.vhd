--==============================================================================
-- KTH Royal Institute of Technology Stockholm
-- PoGO+ minion board gateware
--==============================================================================
--
-- author: Theodor Stana (stana@kth.se)
--
-- date of creation: 2015-10-06
--
-- version: 1.0
--
-- description:
--
--==============================================================================
-- GNU LESSER GENERAL PUBLIC LICENSE
--==============================================================================
-- This source file is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by the
-- Free Software Foundation; either version 2.1 of the License, or (at your
-- option) any later version. This source is distributed in the hope that it
-- will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU Lesser General Public License for more details. You should have
-- received a copy of the GNU Lesser General Public License along with this
-- source; if not, download it from http://www.gnu.org/licenses/lgpl-2.1.html
--==============================================================================
-- last changes:
--    2015-10-06   Theodor Stana     File created
--==============================================================================
-- TODO: -
--==============================================================================

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity minion is
  port
  (
    ---------------------------------------------------------------------------
    -- Clock
    ---------------------------------------------------------------------------
    clk_i          : in  std_logic;

    ---------------------------------------------------------------------------
    -- FADC & DIO side ports
    ---------------------------------------------------------------------------
    -- FADC -> DIO
    fadc1_i         : in  std_logic_vector(7 downto 0);
    fadc2_i         : in  std_logic_vector(7 downto 0);
    fadc3_i         : in  std_logic_vector(7 downto 0);
    fadc4_i         : in  std_logic_vector(7 downto 0);
    fadc5_i         : in  std_logic_vector(7 downto 0);
    fadc6_i         : in  std_logic_vector(7 downto 0);

    dio_o           : out std_logic_vector(7 downto 0);

    -- DIO -> FADC
    dio_i           : in  std_logic_vector(7 downto 0);

    fadc1_o         : out std_logic_vector(7 downto 0);
    fadc2_o         : out std_logic_vector(7 downto 0);
    fadc3_o         : out std_logic_vector(7 downto 0);
    fadc4_o         : out std_logic_vector(7 downto 0);
    fadc5_o         : out std_logic_vector(7 downto 0);
    fadc6_o         : out std_logic_vector(7 downto 0);

    ---------------------------------------------------------------------------
    -- Ports to the power board
    ---------------------------------------------------------------------------
    fadc_pwr_en_o  : out std_logic_vector(5 downto 0);
    pmt_pwr_en_o   : out std_logic_vector(5 downto 0);
    dio_pwr_en_o   : out std_logic;
    spwrt_pwr_en_o : out std_logic;
    sp3_pwr_en_o   : out std_logic
  );
end entity minion;

architecture behav of minion is

  --===========================================================================
  -- Attributes
  --===========================================================================
  attribute keep : boolean;

  --===========================================================================
  -- Signals
  --===========================================================================
  signal fadc_pwr_en    : std_logic_vector(5 downto 0);
  signal pmt_pwr_en     : std_logic_vector(5 downto 0);
  signal dio_pwr_en     : std_logic;
  signal spwrt_pwr_en   : std_logic;
  signal sp3_pwr_en     : std_logic;

  signal trig_in        : std_logic_vector(5 downto 0);
  signal writing_in     : std_logic_vector(5 downto 0);
  signal wd_in          : std_logic_vector(5 downto 0);
  signal ud_in          : std_logic_vector(5 downto 0);
  signal hit_in         : std_logic_vector(5 downto 0);

  signal do_write_in    : std_logic;
  signal pseudo_pps_in  : std_logic;
  signal stop_in        : std_logic;

  signal trig_or        : std_logic;
  signal writing_or     : std_logic;
  signal writing_and    : std_logic;
  signal wd_or          : std_logic;
  signal ud_or          : std_logic;

  signal do_write_out   : std_logic_vector(5 downto 0);
  signal pseudo_pps_out : std_logic_vector(5 downto 0);
  signal stop_out       : std_logic_vector(5 downto 0);

--=============================================================================
-- architecture begin
--=============================================================================
begin

 --===========================================================================
  -- LVDS inputs to internal signal assignments
  --===========================================================================
  -- Inputs from FADCs
  trig_in(0)    <= fadc1_i(0);
  trig_in(1)    <= fadc2_i(0);
  trig_in(2)    <= fadc3_i(0);
  trig_in(3)    <= fadc4_i(0);
  trig_in(4)    <= fadc5_i(0);
  trig_in(5)    <= fadc6_i(0);

  writing_in(0) <= fadc1_i(1);
  writing_in(1) <= fadc2_i(1);
  writing_in(2) <= fadc3_i(1);
  writing_in(3) <= fadc4_i(1);
  writing_in(4) <= fadc5_i(1);
  writing_in(5) <= fadc6_i(1);

  wd_in(0)      <= fadc1_i(2);
  wd_in(1)      <= fadc2_i(2);
  wd_in(2)      <= fadc3_i(2);
  wd_in(3)      <= fadc4_i(2);
  wd_in(4)      <= fadc5_i(2);
  wd_in(5)      <= fadc6_i(2);

  ud_in(0)      <= fadc1_i(3);
  ud_in(1)      <= fadc2_i(3);
  ud_in(2)      <= fadc3_i(3);
  ud_in(3)      <= fadc4_i(3);
  ud_in(4)      <= fadc5_i(3);
  ud_in(5)      <= fadc6_i(3);

  hit_in(0)     <= fadc1_i(4);
  hit_in(1)     <= fadc2_i(4);
  hit_in(2)     <= fadc3_i(4);
  hit_in(3)     <= fadc4_i(4);
  hit_in(4)     <= fadc5_i(4);
  hit_in(5)     <= fadc6_i(4);

  -- Inputs from DIO
  pseudo_pps_in <= dio_i(0);
  do_write_in   <= dio_i(1);
  stop_in       <= dio_i(2);

  --===========================================================================
  -- FADC -> DIO outputs
  --===========================================================================
  -- Trigger OR
  trig_or <= trig_in(0) or trig_in(1); --'1' when (trig_in /= (trig_in'range => '0')) else '0';

  -- FADCs writing
  writing_and <= writing_in(0) and writing_in(1); -- '1' when (writing_in  = (writing_in'range => '1')) else '0';
  writing_or  <= writing_in(0) or writing_in(1);  -- '1' when (writing_in /= (writing_in'range => '0')) else '0';

  -- Upper Discrimination (UD) & Waveform Discrimination (WD) veto signals
--  ud_or  <= '1' when (ud_in /= (ud_in'range => '0')) else '0';
--  wd_or  <= '1' when (wd_in /= (wd_in'range => '0')) else '0';

  -- Assign the outputs
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      dio_o(0) <= trig_or;
      dio_o(1) <= writing_or;
      dio_o(2) <= '0'; -- writing_and;
      dio_o(3) <= '0'; -- ud_or;
      dio_o(4) <= '0'; -- wd_or;
      dio_o(5) <= '0'; -- pps_out;
      dio_o(6) <= '0';
      dio_o(7) <= '0';
    end if;
  end process;

  --===========================================================================
  -- DIO -> FADC outputs
  --===========================================================================
  -- Fan-out to ports
  process (clk_i)
  begin
    if rising_edge(clk_i) then
      do_write_out   <= (do_write_out'range   => do_write_in);
      pseudo_pps_out <= (pseudo_pps_out'range => pseudo_pps_in);
      stop_out       <= (stop_out'range       => stop_in);
    end if;
  end process;

  -- Port assignments
  fadc1_o(0) <= pseudo_pps_out(0);
  fadc1_o(1) <= do_write_out(0);
  fadc1_o(2) <= stop_out(0);
  fadc1_o(3) <= '0';
  fadc1_o(4) <= '0';
  fadc1_o(5) <= '0';
  fadc1_o(6) <= '0';
  fadc1_o(7) <= '0';

  fadc2_o(0) <= pseudo_pps_out(1);
  fadc2_o(1) <= do_write_out(1);
  fadc2_o(2) <= stop_out(1);
  fadc2_o(3) <= '0';
  fadc2_o(4) <= '0';
  fadc2_o(5) <= '0';
  fadc2_o(6) <= '0';
  fadc2_o(7) <= '0';

  fadc3_o(0) <= pseudo_pps_out(2);
  fadc3_o(1) <= do_write_out(2);
  fadc3_o(2) <= stop_out(2);
  fadc3_o(3) <= '0';
  fadc3_o(4) <= '0';
  fadc3_o(5) <= '0';
  fadc3_o(6) <= '0';
  fadc3_o(7) <= '0';

  fadc4_o(0) <= pseudo_pps_out(3);
  fadc4_o(1) <= do_write_out(3);
  fadc4_o(2) <= stop_out(3);
  fadc4_o(3) <= '0';
  fadc4_o(4) <= '0';
  fadc4_o(5) <= '0';
  fadc4_o(6) <= '0';
  fadc4_o(7) <= '0';

  fadc5_o(0) <= pseudo_pps_out(4);
  fadc5_o(1) <= do_write_out(4);
  fadc5_o(2) <= stop_out(4);
  fadc5_o(3) <= '0';
  fadc5_o(4) <= '0';
  fadc5_o(5) <= '0';
  fadc5_o(6) <= '0';
  fadc5_o(7) <= '0';

  fadc6_o(0) <= pseudo_pps_out(5);
  fadc6_o(1) <= do_write_out(5);
  fadc6_o(2) <= stop_out(5);
  fadc6_o(3) <= '0';
  fadc6_o(4) <= '0';
  fadc6_o(5) <= '0';
  fadc6_o(6) <= '0';
  fadc6_o(7) <= '0';

  --===========================================================================
  -- Power board control
  --===========================================================================
  fadc_pwr_en_o  <= (others => '1'); -- fadc1_i(5 downto 0);
  pmt_pwr_en_o   <= (others => '1'); -- fadc1_i(5 downto 0);
  spwrt_pwr_en_o <= '1'; -- fadc1_i(6);
  sp3_pwr_en_o   <= '1'; -- fadc1_i(6);
  dio_pwr_en_o   <= '1'; -- fadc1_i(7);

end architecture behav;
--=============================================================================
-- architecture end
--=============================================================================
