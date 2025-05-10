pragma SPARK_Mode (On);

with Ada.Text_IO; use Ada.Text_IO;
with PIN;
with MyString;
with MyStringTokeniser;
with Ada.Long_Long_Integer_Text_IO;

package body CalculatorCommands is
   --package Lines is new MyString (Max_MyString_Length => 2048);
   procedure Handle_Unlock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in PIN.PIN;
      Unlocked  : in out Boolean) is
   begin
      if T'First > 2 or else T'Last < 2 or else T (2).Length = 0 then
         Put_Line ("Usage: unlock <pin>");
      else
         declare
            RawPIN : constant String := Get_Token_String (S, T (2));
         begin
            if RawPIN'Length /= 4
              or else (for some C of RawPIN => C not in '0' .. '9')
            then
               Put_Line ("Invalid PIN: must be a 4-digit numeric string.");
            elsif Unlocked then
               Put_Line ("Already unlocked.");
            elsif PIN."=" (PIN.From_String (RawPIN), MasterPIN) then
               Unlocked := True;
               Put_Line ("Unlocked successfully.");
            else
               Put_Line ("Incorrect PIN. Still locked.");
            end if;
         end;
      end if;

   end Handle_Unlock;
   procedure Handle_Lock
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      MasterPIN : in out PIN.PIN;
      Unlocked  : in out Boolean) is
   begin
      if T'First > 2 or else T'Last < 2 or else T (2).Length = 0 then
         Put_Line ("Usage: lock <newpin>");
      else
         declare
            RawPIN : constant String := Get_Token_String (S, T (2));
         begin
            if RawPIN'Length /= 4
              or else (for some C of RawPIN => C not in '0' .. '9')
            then
               Put_Line ("Invalid PIN: must be a 4-digit numeric string.");
            elsif not Unlocked then
               Put_Line ("Already locked.");
            else
               MasterPIN := PIN.From_String (RawPIN);
               Unlocked := False;
               Put_Line ("Locked with new PIN.");
            end if;
         end;
      end if;
   end Handle_Lock;

   procedure Handle_Push1
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Unlocked  : in Boolean;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural)
   is
      Max_Len : constant := 20;
   begin
      if not Unlocked then
         Put_Line ("Command not allowed: calculator is locked.");
      elsif T'First > 2 or else T'Last < 2 or else T (2).Length = 0 then
         Put_Line ("Usage: push1 <number>");
      elsif Stack_Top >= Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         declare
            RawVal : constant String := Get_Token_String (S, T (2));
         begin
            if RawVal'Length = 0 or else RawVal'Length > Max_Len then
               Put_Line ("Invalid number.");
            else
               declare
                  Val : constant Integer :=
                    StringToInteger.From_String (RawVal);
               begin
                  Stack_Top := Stack_Top + 1;
                  Stack (Stack_Top) := Val;
                  Put_Line
                    ("Pushed " & Integer'Image (Val) & " onto the stack.");
               end;
            end if;
         end;
      end if;
   end Handle_Push1;

   function Get_Token_String
     (S : Lines.MyString; Tok : MyStringTokeniser.TokenExtent) return String
   is
      Start_Pos : constant Positive := Tok.Start;
      Token_Len : constant Natural := Tok.Length;
      Temp_End  : Long_Long_Integer;
      End_Pos   : Natural;
   begin
      if Token_Len = 0 then
         return "";
      end if;

      Temp_End :=
        Long_Long_Integer (Start_Pos) + Long_Long_Integer (Token_Len) - 1;

      if Temp_End > Long_Long_Integer (Lines.Length (S))
        or else Temp_End > Long_Long_Integer (Natural'Last)
        or else Temp_End < Long_Long_Integer (Start_Pos)
      then
         return "";
      end if;

      End_Pos := Natural (Temp_End);

      return Lines.To_String (Lines.Substring (S, Start_Pos, End_Pos));
   end Get_Token_String;

end CalculatorCommands;
