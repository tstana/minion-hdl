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
  generic
  (
    g_num_fadc_boards  : natural := 6
  );
  port
  (
    ---------------------------------------------------------------------------
    -- FADC & DIO side ports
    ---------------------------------------------------------------------------
    -- Trigger and one-hit ports
    clk_i          : in  std_logic;
    onehit_en_i    : in  std_logic;
    trig_i         : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    hit_i          : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    trig_or_o      : out std_logic;

    reset_veto_i   : in  std_logic;
    reset_veto_o   : out std_logic_vector(g_num_fadc_boards-1 downto 0);

    -- FADC -> DIO
    writing_i      : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    writing_or_o   : out std_logic;
    writing_and_o  : out std_logic;

    ud_i           : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    ud_or_o        : out std_logic;

    wd_i           : in  std_logic_vector(g_num_fadc_boards-1 downto 0);
    wd_or_o        : out std_logic;

    -- DIO -> FADC
    do_write_i     : in  std_logic;
    do_write_o     : out std_logic_vector(g_num_fadc_boards-1 downto 0);

    pps_pseudo_i   : in  std_logic;
    pps_pseudo_o   : out std_logic_vector(g_num_fadc_boards-1 downto 0);

    start_stop_i   : in  std_logic;
    start_stop_o   : out std_logic_vector(g_num_fadc_boards-1 downto 0);

    ---------------------------------------------------------------------------
    -- IUB side ports
    ---------------------------------------------------------------------------
    -- Data shift ports
    clk_shift_i    : in  std_logic;
    clk_read_i     : in  std_logic;
    data_i         : in  std_logic;

    -- Temperature MUX-ing ports
    temp_i         : in  std_logic_vector(18 downto 0);
    temp_o         : out std_logic;

    ---------------------------------------------------------------------------
    -- Ports to the power board
    ---------------------------------------------------------------------------
    fadc_pwr_en_o  : out std_logic_vector(g_num_fadc_boards-1 downto 0);
    pmt_pwr_en_o   : out std_logic_vector(g_num_fadc_boards-1 downto 0);
    dio_pwr_en_o   : out std_logic;
    spwrt_pwr_en_o : out std_logic;
    sp3_pwr_en_o   : out std_logic
  );
end entity minion;

architecture behav of minion is

  --===========================================================================
  -- Functions
  --===========================================================================

  --===========================================================================
  -- Types
  --===========================================================================

  --===========================================================================
  -- Signals
  --===========================================================================
  signal sh_reg         : std_logic_vector(39 downto 0);
  signal data_from_iub  : std_logic_vector(39 downto 0);

  signal temp_sel       : std_logic_vector( 4 downto 0);

  signal fadc_pwr_en    : std_logic_vector(g_num_fadc_boards-1 downto 0);
  signal pmt_pwr_en     : std_logic_vector(g_num_fadc_boards-1 downto 0);
  signal dio_pwr_en     : std_logic;
  signal spwrt_pwr_en   : std_logic;
  signal sp3_pwr_en     : std_logic;

--=============================================================================
-- architecture begin
--=============================================================================
begin

  --===========================================================================
  -- Data input from IUB
  --===========================================================================
  -- Shift and data storage registers, controlled by signals from the IUB
  p_shift_reg : process(clk_shift_i)
  begin
    if rising_edge(clk_shift_i) then
      sh_reg <= data_i & sh_reg(39 downto 1);
    end if;
  end process p_shift_reg;

  p_data_reg : process(clk_read_i)
  begin
    if rising_edge(clk_read_i) then
      data_from_iub <= sh_reg;
    end if;
  end process p_data_reg;

  -- Split IUB data into relevant fields
  temp_sel     <= data_from_iub( 4 downto  0);
  fadc_pwr_en  <= data_from_iub( 5+(g_num_fadc_boards-1) downto  5);
  dio_pwr_en   <= data_from_iub(17);
  spwrt_pwr_en <= data_from_iub(18);
  sp3_pwr_en   <= data_from_iub(20);
  pmt_pwr_en   <= data_from_iub(21+(g_num_fadc_boards-1) downto 21);

  --===========================================================================
  -- Temperature MUX output assignment
  --===========================================================================
  temp_o <= temp_i(to_integer(unsigned(temp_sel)));

  --===========================================================================
  -- Power enable outputs assignment
  --===========================================================================
  fadc_pwr_en_o  <= fadc_pwr_en;
  pmt_pwr_en_o   <= pmt_pwr_en;
  dio_pwr_en_o   <= dio_pwr_en;
  spwrt_pwr_en_o <= spwrt_pwr_en;
  sp3_pwr_en_o   <= sp3_pwr_en;

  --===========================================================================
  -- FADC -> DIO outputs
  --===========================================================================
  -- FADCs writing
  writing_and_o <= '1' when (writing_i  = (writing_i'range => '1')) else '0';
  writing_or_o  <= '1' when (writing_i /= (writing_i'range => '0')) else '0';

  -- Upper Discrimination (UD) & Waveform Discrimination (WD) veto signals
  ud_or_o  <= '1' when (ud_i /= (ud_i'range => '0')) else '0';
  wd_or_o  <= '1' when (wd_i /= (wd_i'range => '0')) else '0';

  --===========================================================================
  -- DIO -> FADC outputs
  --===========================================================================
  do_write_o   <= (do_write_o'range   => do_write_i);
  pps_pseudo_o <= (pps_pseudo_o'range => pps_pseudo_i);
  start_stop_o <= (start_stop_o'range => start_stop_i);
  reset_veto_o <= (reset_veto_o'range => reset_veto_i);

end architecture behav;
--=============================================================================
-- architecture end
--=============================================================================
