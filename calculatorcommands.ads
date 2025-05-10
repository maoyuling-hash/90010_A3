pragma SPARK_Mode (On);

with PIN;
with MyStringTokeniser;

package CalculatorCommands is

   procedure Handle_Unlock
     (MasterPIN  : in PIN.PIN;
      CurrentPIN : in PIN.PIN;
      Unlocked   : in out Boolean);

   procedure Handle_Lock
     (NewPIN    : in PIN.PIN;
      MasterPIN : in out PIN.PIN;
      Unlocked  : in out Boolean);

end CalculatorCommands;
