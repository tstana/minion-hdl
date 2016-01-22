library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity lvds_dio is
  port
  (
    lvds_i : in  std_logic_vector(15 downto 0);
    lvds_o : out std_logic_vector(31 downto 0)
  );
end lvds_dio;

architecture arch of lvds_dio is

begin

  lvds_o(15 downto  0) <= lvds_i(15 downto 0);
  lvds_o(31 downto 16) <= (others => '0');

end arch;

