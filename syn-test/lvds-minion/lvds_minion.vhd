library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity lvds_minion is
  port
  (
    fadc1_i : in  std_logic_vector(7 downto 0);
    fadc1_o : out std_logic_vector(7 downto 0);
    fadc2_i : in  std_logic_vector(7 downto 0);
    fadc2_o : out std_logic_vector(7 downto 0);
    fadc3_i : in  std_logic_vector(7 downto 0);
    fadc3_o : out std_logic_vector(7 downto 0);
    fadc4_i : in  std_logic_vector(7 downto 0);
    fadc4_o : out std_logic_vector(7 downto 0);
    fadc5_i : in  std_logic_vector(7 downto 0);
    fadc5_o : out std_logic_vector(7 downto 0);
    fadc6_i : in  std_logic_vector(7 downto 0);
    fadc6_o : out std_logic_vector(7 downto 0);
    dio_i   : in  std_logic_vector(7 downto 0);
    dio_o   : out std_logic_vector(7 downto 0)
  );
end lvds_minion;

architecture arch of lvds_minion is

begin

  fadc1_o <= fadc1_i;
  fadc2_o <= fadc2_i;
  fadc3_o <= fadc3_i;
  fadc4_o <= fadc4_i;
  fadc5_o <= fadc5_i;
  fadc6_o <= dio_i;
  dio_o   <= fadc6_i;

end arch;

