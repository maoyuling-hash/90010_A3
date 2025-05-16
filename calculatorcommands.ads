pragma SPARK_Mode (On);

with PIN;
with MyStringTokeniser;
with MyString;
with Ada.Long_Long_Integer_Text_IO;
with MemoryStore;
with StringToInteger;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package CalculatorCommands
  with SPARK_Mode
is
   package Lines is new MyString (Max_MyString_Length => 2048);
   Max_Stack_Size : constant := 512;
   type Operand_Stack is array (1 .. Max_Stack_Size) of Integer;
   procedure Handle_Unlock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in PIN.PIN;
      Unlocked  : in out Boolean;
      Numtokens : in Natural);

   procedure Handle_Lock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in out PIN.PIN;
      Unlocked  : in out Boolean;
      Numtokens : in Natural);

   procedure Handle_Push1
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Numtokens : in Natural);

   procedure Handle_Push2
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      NumTokens : in Natural);

   procedure Handle_Pop
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      NumTokens : in Natural);

   procedure Handle_Add
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   with Pre => (Unlocked);
   procedure Handle_Subtract
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   with Pre => (Unlocked);

   procedure Handle_Multiply
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   with Pre => (Unlocked);

   procedure Handle_Divide
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   with Pre => (Unlocked);

   procedure Handle_StoreTo
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in out MemoryStore.Database)
   with Pre => (Unlocked);

   procedure Handle_List
     (Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in MemoryStore.Database)
   with Pre => (Unlocked);

   procedure Handle_Remove
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in out MemoryStore.Database)
   with Pre => (Unlocked);

   procedure Handle_LoadFrom
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in MemoryStore.Database)
   with Pre => (Unlocked);
   function Get_Token_String
     (S : Lines.MyString; Tok : MyStringTokeniser.TokenExtent) return String;

   function Trim_Trailing_Spaces (Input : in String) return String;

end CalculatorCommands;
