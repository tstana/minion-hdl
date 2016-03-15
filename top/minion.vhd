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
--    2015-10-06   Theodor Stana    File created
--    2016-02-04   Theodor Stana    Changes according to new DIO and FADC code
--==============================================================================
-- TODO: see below
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
    -- IUB side ports
    ---------------------------------------------------------------------------
    -- Data shift ports
    iub_shift_i    : in  std_logic;
    iub_read_i     : in  std_logic;
    iub_data_i     : in  std_logic;

    -- Temperature MUX-ing ports
    temp_i         : in  std_logic_vector(15 downto 0);
    iub_temp_o     : out std_logic;

    -- PPS port IUB -> DIO
    iub_pps_i      : in  std_logic;
    pps_o          : out std_logic;

    -- Backup signals to the IUB
    iub_bkp1_o      : out std_logic;
    iub_bkp2_o      : out std_logic;
    iub_bkp3_o      : out std_logic;

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
  -- Functions
  --===========================================================================
  -- TODO: Optimize me!
  function f_count_ones(v : std_logic_vector) return unsigned is
      variable n : unsigned(3-1 downto 0);
  begin
    n := (others => '0');
    for i in v'high downto 0 loop
      if v(i) = '1' then
        n := n+1;
      end if;
    end loop;
    return n;
  end function;

  --===========================================================================
  -- Constants
  --===========================================================================
  -- Reset period in clock cycles: 5000000*20ns = 100 ms
  constant c_reset_per      : natural := 4999999;

  --===========================================================================
  -- Signals
  --===========================================================================
  signal reset              : std_logic := '0';
  signal reset_count_dis    : std_logic := '0';
  signal reset_count        : unsigned(22 downto 0) := (others => '0');

  signal iub_shift_d0       : std_logic;
  signal iub_shift_d1       : std_logic;
  signal iub_shift_d2       : std_logic;
  signal iub_shift_fedge_p0 : std_logic;
  signal read_dly           : std_logic_vector(39 downto 0);
  signal sh_reg             : std_logic_vector(39 downto 0);
  signal data_from_iub      : std_logic_vector(39 downto 0);

  signal temp_sel           : std_logic_vector( 3 downto 0);

  signal pps_out            : std_logic;

  signal fadc_pwr_en        : std_logic_vector(5 downto 0);
  signal pmt_pwr_en         : std_logic_vector(5 downto 0);
  signal dio_pwr_en         : std_logic;
  signal spwrt_pwr_en       : std_logic;
  signal sp3_pwr_en         : std_logic;

  signal trig_mask          : std_logic_vector(5 downto 0);
  signal writing_and_mask   : std_logic_vector(5 downto 0);
  signal writing_or_mask    : std_logic_vector(5 downto 0);
  signal wd_mask            : std_logic_vector(5 downto 0);
  signal ud_mask            : std_logic_vector(5 downto 0);
  signal hit_mask           : std_logic_vector(5 downto 0);
  signal twohits_mask       : std_logic_vector(5 downto 0);

  signal stop_in            : std_logic;
  signal do_write_in        : std_logic;
  signal pseudo_pps_in      : std_logic;
  signal twohits_out        : std_logic;

  signal trig_or            : std_logic;
  signal writing_or         : std_logic;
  signal writing_and        : std_logic;
  signal hit_or             : std_logic;
  signal wd_or              : std_logic;
  signal ud_or              : std_logic;

  signal stop_out           : std_logic_vector(5 downto 0);
  signal do_write_out       : std_logic_vector(5 downto 0);
  signal pseudo_pps_out     : std_logic_vector(5 downto 0);

  -- TODO: Remove me!
  signal trig               : unsigned(3-1 downto 0);
  signal hit                : unsigned(3-1 downto 0);
  signal sum                : unsigned(3-1 downto 0);

--=============================================================================
-- architecture begin
--=============================================================================
begin

  --===========================================================================
  -- Power-on reset
  --===========================================================================
  p_reset : process (clk_i)
  begin
    if rising_edge(clk_i) then
      if (reset_count_dis = '0') then
        reset_count <= reset_count + 1;
        reset       <= '1';
        if (reset_count = c_reset_per-1) then
          reset_count_dis <= '1';
          reset           <= '0';
        end if;
      end if;
    end if;
  end process p_reset;

  --===========================================================================
  -- IUB control
  --===========================================================================
  -- Locally synchronize and detect falling edge on shift input
  p_shift_fall_edge : process (reset, clk_i)
  begin
    if (reset = '1') then
      iub_shift_d0       <= '0';
      iub_shift_d1       <= '0';
      iub_shift_d2       <= '0';
      iub_shift_fedge_p0 <= '0';
    elsif rising_edge(clk_i) then
      iub_shift_d0       <= iub_shift_i;
      iub_shift_d1       <= iub_shift_d0;
      iub_shift_d2       <= iub_shift_d1;
      iub_shift_fedge_p0 <= iub_shift_d2 and (not iub_shift_d1);
    end if;
  end process p_shift_fall_edge;

  -- Shift and data storage registers, controlled by signals from the IUB
  p_shift_reg : process(reset, clk_i)
  begin
    if (reset = '1') then
      sh_reg        <= (others => '0');
      read_dly      <= (others => '0');
      data_from_iub <= (others => '0');
    elsif rising_edge(clk_i) then
      if (iub_shift_fedge_p0 = '1') then
        sh_reg   <= iub_data_i & sh_reg(39 downto 1);
        read_dly <= iub_read_i & read_dly(39 downto 1);
        if (read_dly(0) = '1') then
          data_from_iub <= sh_reg;
        end if;
      end if;
    end if;
  end process p_shift_reg;

  -- Split IUB data into relevant fields
  temp_sel     <= data_from_iub( 3 downto  0);
  fadc_pwr_en  <= data_from_iub( 9 downto  4);
  sp3_pwr_en   <= data_from_iub(17);
  spwrt_pwr_en <= data_from_iub(18);
  pmt_pwr_en   <= data_from_iub(25 downto 20);
  dio_pwr_en   <= data_from_iub(32);

  -- Temperature MUX output assignment
  -- NOTE: Should not be clocked, since temp sensor output is duty-cycle-encoded
  iub_temp_o <= temp_i(to_integer(unsigned(temp_sel)));

  -- PPS
  pps_out <= iub_pps_i;

  --===========================================================================
  -- LVDS inputs to internal signal assignments
  --===========================================================================
  -- Mask inputs from FADCs with FADC power enable signals
  trig_mask(0)        <= fadc1_i(0) and fadc_pwr_en(0);
  trig_mask(1)        <= fadc2_i(0) and fadc_pwr_en(1);
  trig_mask(2)        <= fadc3_i(0) and fadc_pwr_en(2);
  trig_mask(3)        <= fadc4_i(0) and fadc_pwr_en(3);
  trig_mask(4)        <= fadc5_i(0) and fadc_pwr_en(4);
  trig_mask(5)        <= fadc6_i(0) and fadc_pwr_en(5);

  writing_or_mask(0)  <= fadc1_i(1) and fadc_pwr_en(0);
  writing_or_mask(1)  <= fadc2_i(1) and fadc_pwr_en(1);
  writing_or_mask(2)  <= fadc3_i(1) and fadc_pwr_en(2);
  writing_or_mask(3)  <= fadc4_i(1) and fadc_pwr_en(3);
  writing_or_mask(4)  <= fadc5_i(1) and fadc_pwr_en(4);
  writing_or_mask(5)  <= fadc6_i(1) and fadc_pwr_en(5);

  writing_and_mask(0) <= fadc1_i(1) or (not fadc_pwr_en(0));
  writing_and_mask(1) <= fadc2_i(1) or (not fadc_pwr_en(1));
  writing_and_mask(2) <= fadc3_i(1) or (not fadc_pwr_en(2));
  writing_and_mask(3) <= fadc4_i(1) or (not fadc_pwr_en(3));
  writing_and_mask(4) <= fadc5_i(1) or (not fadc_pwr_en(4));
  writing_and_mask(5) <= fadc6_i(1) or (not fadc_pwr_en(5));

  wd_mask(0)          <= fadc1_i(2) and fadc_pwr_en(0);
  wd_mask(1)          <= fadc2_i(2) and fadc_pwr_en(1);
  wd_mask(2)          <= fadc3_i(2) and fadc_pwr_en(2);
  wd_mask(3)          <= fadc4_i(2) and fadc_pwr_en(3);
  wd_mask(4)          <= fadc5_i(2) and fadc_pwr_en(4);
  wd_mask(5)          <= fadc6_i(2) and fadc_pwr_en(5);

  ud_mask(0)          <= fadc1_i(3) and fadc_pwr_en(0);
  ud_mask(1)          <= fadc2_i(3) and fadc_pwr_en(1);
  ud_mask(2)          <= fadc3_i(3) and fadc_pwr_en(2);
  ud_mask(3)          <= fadc4_i(3) and fadc_pwr_en(3);
  ud_mask(4)          <= fadc5_i(3) and fadc_pwr_en(4);
  ud_mask(5)          <= fadc6_i(3) and fadc_pwr_en(5);

  hit_mask(0)         <= fadc1_i(4) and fadc_pwr_en(0);
  hit_mask(1)         <= fadc2_i(4) and fadc_pwr_en(1);
  hit_mask(2)         <= fadc3_i(4) and fadc_pwr_en(2);
  hit_mask(3)         <= fadc4_i(4) and fadc_pwr_en(3);
  hit_mask(4)         <= fadc5_i(4) and fadc_pwr_en(4);
  hit_mask(5)         <= fadc6_i(4) and fadc_pwr_en(5);

  twohits_mask(0)     <= fadc1_i(5) and fadc_pwr_en(0);
  twohits_mask(1)     <= fadc2_i(5) and fadc_pwr_en(1);
  twohits_mask(2)     <= fadc3_i(5) and fadc_pwr_en(2);
  twohits_mask(3)     <= fadc4_i(5) and fadc_pwr_en(3);
  twohits_mask(4)     <= fadc5_i(5) and fadc_pwr_en(4);
  twohits_mask(5)     <= fadc6_i(5) and fadc_pwr_en(5);

  -- Inputs from DIO
  pseudo_pps_in <= dio_i(0);
  do_write_in   <= dio_i(1);
  stop_in       <= dio_i(2);

  --===========================================================================
  -- One-hit veto implementation
  --===========================================================================
  -- TODO: Remove, this is temporary...
  twohits_out <= '1' when (twohits_mask /= (twohits_mask'range => '0')) else '0';

--  trig <= f_count_ones(trig_mask);
--  hit  <= f_count_ones(hit_mask);
--  sum  <= trig + hit;
--
--  process (clk_i)
--  begin
--    if rising_edge(clk_i) then
--      trig_or <= '0';
--      if (onehit_en = '1') then
--        if (trig > 1) or (hit > 1) or (sum > 1) then
--          trig_or <= '1';
--        end if;
--      else
--        if (trig /= (trig'range => '0')) then
--          trig_or <= '1';
--        end if;
--      end if;
--    end if;
--  end process;

  --===========================================================================
  -- FADC -> DIO outputs
  --===========================================================================
  -- FADC signals
  trig_or     <= '1' when (trig_mask        /= (trig_mask'range        => '0')) else '0';
  writing_and <= '1' when (writing_and_mask  = (writing_and_mask'range => '1')) else '0';
  writing_or  <= '1' when (writing_or_mask  /= (writing_or_mask'range  => '0')) else '0';
  hit_or      <= '1' when (hit_mask         /= (hit_mask'range         => '0')) else '0';

  -- Upper Discrimination (UD) & Waveform Discrimination (WD) veto signals
  ud_or <= '1' when (ud_mask /= (ud_mask'range => '0')) else '0';
  wd_or <= '1' when (wd_mask /= (wd_mask'range => '0')) else '0';

  -- Assign the outputs
  process (reset, clk_i)
  begin
    if (reset = '1') then
      dio_o <= (others => '0');
    elsif rising_edge(clk_i) then
      dio_o(0) <= trig_or;
      dio_o(1) <= writing_or;
      dio_o(2) <= writing_and;
      dio_o(3) <= hit_or;
      dio_o(4) <= wd_or;
      dio_o(5) <= ud_or;
      dio_o(6) <= twohits_out;
      dio_o(7) <= pps_out;
    end if;
  end process;

  --===========================================================================
  -- DIO -> FADC outputs
  --===========================================================================
  -- Fan-out to ports
  process (reset, clk_i)
  begin
    if (reset = '1') then
      pseudo_pps_out <= (others => '0');
      do_write_out   <= (others => '0');
      stop_out       <= (others => '0');
    elsif rising_edge(clk_i) then
      pseudo_pps_out <= (pseudo_pps_out'range => pseudo_pps_in);
      do_write_out   <= (do_write_out'range   => do_write_in);
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
  -- Power enable outputs assignment
  --===========================================================================
  process (reset, clk_i)
  begin
    if (reset = '1') then
      fadc_pwr_en_o  <= (others => '0');
      pmt_pwr_en_o   <= (others => '0');
      dio_pwr_en_o   <= '0';
      spwrt_pwr_en_o <= '0';
      sp3_pwr_en_o   <= '0';
    elsif rising_edge(clk_i) then
      fadc_pwr_en_o  <= fadc_pwr_en;
      pmt_pwr_en_o   <= pmt_pwr_en;
      dio_pwr_en_o   <= dio_pwr_en;
      spwrt_pwr_en_o <= spwrt_pwr_en;
      sp3_pwr_en_o   <= sp3_pwr_en;
    end if;
  end process;

end architecture behav;
--=============================================================================
-- architecture end
--=============================================================================
