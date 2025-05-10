pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with PIN;

package body CalculatorCommands is

   procedure Handle_Unlock
     (MasterPIN  : in PIN.PIN;
      CurrentPIN : in PIN.PIN;
      Unlocked   : in out Boolean) is
   begin
      if Unlocked then
         Put_Line ("Already unlocked.");
      else
         if PIN."=" (CurrentPIN, MasterPIN) then
            Unlocked := True;
         else
            Put_Line ("Incorrect PIN. Still locked.");
         end if;
      end if;
   end Handle_Unlock;

   procedure Handle_Lock
     (NewPIN    : in PIN.PIN;
      MasterPIN : in out PIN.PIN;
      Unlocked  : in out Boolean) is
   begin
      if not Unlocked then
         Put_Line ("Already locked.");
      else
         MasterPIN := NewPIN;
         Unlocked := False;
         Put_Line ("Locked with new PIN.");
      end if;
   end Handle_Lock;

end CalculatorCommands;
