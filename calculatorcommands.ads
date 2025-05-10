pragma SPARK_Mode (On);

with PIN;
with MyStringTokeniser;
with MyString;
with Ada.Long_Long_Integer_Text_IO;
with StringToInteger;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package CalculatorCommands is
   package Lines is new MyString (Max_MyString_Length => 2048);
   Max_Stack_Size : constant := 512;
   type Operand_Stack is array (1 .. Max_Stack_Size) of Integer;
   procedure Handle_Unlock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in PIN.PIN;
      Unlocked  : in out Boolean);

   procedure Handle_Lock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in out PIN.PIN;
      Unlocked  : in out Boolean);

   procedure Handle_Push1
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Unlocked  : in Boolean;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural);

   function Get_Token_String
     (S : Lines.MyString; Tok : MyStringTokeniser.TokenExtent) return String;

end CalculatorCommands;
