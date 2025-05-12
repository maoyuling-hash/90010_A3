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
      Unlocked  : in out Boolean;
      Numtokens : in Natural) is
   begin
      if T'First > 2 or else T'Last < 2 or else NumTokens /= 2 then
         Put_Line ("Usage: unlock <pin>");
      else
         declare
            RawPIN : constant String := Get_Token_String (S, T (2));
         begin
            if RawPIN'Length /= 4
              or else (for some C of RawPIN => C not in '0' .. '9')
            then
               Put_Line ("Invalid PIN: must be a 4-digit numeric string.");
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
      Unlocked  : in out Boolean;
      Numtokens : in Natural) is
   begin
      if T'First > 2 or else T'Last < 2 or else NumTokens /= 2 then
         Put_Line ("Usage: lock <newpin>");
      else
         declare
            RawPIN : constant String := Get_Token_String (S, T (2));
         begin
            if RawPIN'Length /= 4
              or else (for some C of RawPIN => C not in '0' .. '9')
            then
               Put_Line ("Invalid PIN: must be a 4-digit numeric string.");
            else
               if PIN."=" (PIN.From_String (RawPIN), MasterPIN) then
                  Put_Line ("Invalid PIN: must be different to old PIN.");
               else
                  MasterPIN := PIN.From_String (RawPIN);
                  Unlocked := False;
                  Put_Line ("Locked with new PIN.");
               end if;
            end if;
         end;
      end if;
   end Handle_Lock;

   procedure Handle_Push1
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Numtokens : in Natural)
   is
      Max_Len : constant := 20;
   begin
      if T'First > 2 or else T'Last < 2 or else NumTokens /= 2 then
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

   procedure Handle_Push2
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      NumTokens : in Natural)
   is
      Max_Len : constant := 20;
   begin
      if NumTokens /= 3 or else T'First > 2 or else T'Last < 3 then
         Put_Line ("Usage: push2 <number1> <number2>");
      elsif Stack_Top > Max_Stack_Size - 2 then
         Put_Line ("Error: stack overflow.");
      elsif T (2).Start < 1
        or else T (2).Length < 1
        or else T (3).Start < 1
        or else T (3).Length < 1
      then
         Put_Line ("Invalid operand tokens.");
      else
         declare
            RawVal1 : constant String := Get_Token_String (S, T (2));
            RawVal2 : constant String := Get_Token_String (S, T (3));
         begin
            if RawVal1'Length = 0
              or else RawVal1'Length > Max_Len
              or else RawVal2'Length = 0
              or else RawVal2'Length > Max_Len
            then
               Put_Line ("Invalid number.");
            else
               declare
                  Val1 : constant Integer :=
                    StringToInteger.From_String (RawVal1);
                  Val2 : constant Integer :=
                    StringToInteger.From_String (RawVal2);
               begin
                  Stack_Top := Stack_Top + 1;
                  Stack (Stack_Top) := Val1;
                  Stack_Top := Stack_Top + 1;
                  Stack (Stack_Top) := Val2;
                  Put_Line
                    ("Pushed "
                     & Integer'Image (Val1)
                     & " and "
                     & Integer'Image (Val2)
                     & " onto the stack.");
               end;
            end if;
         end;
      end if;
   end Handle_Push2;

   procedure Handle_Pop
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      NumTokens : in Natural)
   is
      Popped_Val : Integer;
   begin
      if Stack_Top = 0 then
         Put_Line ("Error: stack underflow.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      elsif NumTokens /= 1 then
         Put_Line ("Usage: pop");
      else
         Popped_Val := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;
         Put_Line
           ("Popped " & Integer'Image (Popped_Val) & " from the stack.");
      end if;
   end Handle_Pop;

   procedure Handle_Add
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   is
      A, B, Result : Integer;
   begin
      if Stack_Top < 2 then
         Put_Line ("Error: not enough operands.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         B := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;
         A := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;

         -- 防止加法溢出
         if A > 0 and then B > Integer'Last - A then
            Put_Line ("Error: integer overflow.");
            return;
         elsif A < 0 and then B < Integer'First - A then
            Put_Line ("Error: integer overflow.");
            return;
         end if;

         Result := A + B;
         Stack_Top := Stack_Top + 1;
         Stack (Stack_Top) := Result;

         Put_Line
           ("Computed: "
            & Integer'Image (A)
            & " + "
            & Integer'Image (B)
            & " = "
            & Integer'Image (Result));
      end if;

   end Handle_Add;

   procedure Handle_Subtract
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   is
      A, B, Result : Integer;
   begin
      if Stack_Top < 2 then
         Put_Line ("Error: not enough operands.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         B := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;
         A := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;

         -- 防止减法溢出
         if B < 0 and then A > Integer'Last + B then
            Put_Line ("Error: integer overflow.");
            return;
         elsif B > 0 and then A < Integer'First + B then
            Put_Line ("Error: integer overflow.");
            return;
         end if;

         Result := A - B;
         Stack_Top := Stack_Top + 1;
         Stack (Stack_Top) := Result;

         Put_Line
           ("Computed: "
            & Integer'Image (A)
            & " - "
            & Integer'Image (B)
            & " = "
            & Integer'Image (Result));
      end if;
   end Handle_Subtract;

   procedure Handle_Multiply
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   is
      A, B, Result : Integer;
   begin
      if Stack_Top < 2 then
         Put_Line ("Error: not enough operands.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         B := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;
         A := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;

         declare
            Wide_A      : Long_Long_Integer := Long_Long_Integer (A);
            Wide_B      : Long_Long_Integer := Long_Long_Integer (B);
            Wide_Result : Long_Long_Integer := Wide_A * Wide_B;
         begin
            if Wide_Result > Long_Long_Integer (Integer'Last)
              or else Wide_Result < Long_Long_Integer (Integer'First)
            then
               Put_Line ("Error: integer overflow.");
               return;
            else
               Result := Integer (Wide_Result);
               Stack_Top := Stack_Top + 1;
               Stack (Stack_Top) := Result;

               Put_Line
                 ("Computed: "
                  & Integer'Image (A)
                  & " * "
                  & Integer'Image (B)
                  & " = "
                  & Integer'Image (Result));
            end if;
         end;
      end if;
   end Handle_Multiply;

   procedure Handle_Divide
     (Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean)
   is
      A, B, Result : Integer;
   begin
      if Stack_Top < 2 then
         Put_Line ("Error: not enough operands.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         B := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;
         A := Stack (Stack_Top);
         Stack (Stack_Top) := 0;
         Stack_Top := Stack_Top - 1;

         -- 防止除法异常（除以 0，或 Integer'First / -1）
         if B = 0 then
            Put_Line ("Error: division by zero.");
            return;
         elsif A = Integer'First and then B = -1 then
            Put_Line ("Error: integer overflow.");
            return;
         end if;

         Result := A / B;
         Stack_Top := Stack_Top + 1;
         Stack (Stack_Top) := Result;

         Put_Line
           ("Computed: "
            & Integer'Image (A)
            & " / "
            & Integer'Image (B)
            & " = "
            & Integer'Image (Result));
      end if;
   end Handle_Divide;

   procedure Handle_StoreTo
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in out MemoryStore.Database)
   is
      RawIndex_Int : Integer;
   begin
      if NumTokens /= 2 or else T'First > 2 or else T'Last < 2 then
         Put_Line ("Usage: storeto <index>");
      elsif T (2).Start < 1 or else T (2).Length < 1 then
         Put_Line ("Invalid token.");
      elsif Stack_Top < 1 then
         Put_Line ("Error: not enough operands.");
      elsif Stack_Top > Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         declare
            RawIndex_Str : constant String := Get_Token_String (S, T (2));
         begin
            if RawIndex_Str'Length = 0 then
               Put_Line ("Invalid index.");
            else
               RawIndex_Int := StringToInteger.From_String (RawIndex_Str);
               if RawIndex_Int < 1 or else RawIndex_Int > 255 then
                  Put_Line ("Index out of range.");
               else
                  declare
                     Loc : MemoryStore.Location_Index :=
                       MemoryStore.Location_Index (RawIndex_Int);
                     Val : MemoryStore.Int32 :=
                       MemoryStore.Int32 (Stack (Stack_Top));
                  begin
                     Stack (Stack_Top) := 0;
                     Stack_Top := Stack_Top - 1;
                     MemoryStore.Put (Mem, Loc, Val);

                     Put_Line
                       ("Stored "
                        & Integer'Image (Integer (Val))
                        & " to location "
                        & Integer'Image (RawIndex_Int));
                  end;
               end if;
            end if;
         end;
      end if;

   end Handle_StoreTo;

   procedure Handle_List
     (Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in MemoryStore.Database) is
   begin
      if NumTokens /= 1 then
         Put_Line ("Usage: list");
      else
         Put_Line ("Memory contents:");
         MemoryStore.Print (Mem);
      end if;
   end Handle_List;
   procedure Handle_Remove
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in out MemoryStore.Database)
   is
      RawIndex_Int : Integer;
   begin
      if NumTokens /= 2 or else T'First > 2 or else T'Last < 2 then
         Put_Line ("Usage: remove <index>");
      else
         declare
            RawIndex_Str : constant String := Get_Token_String (S, T (2));
         begin
            if RawIndex_Str'Length = 0 then
               Put_Line ("Invalid index.");
            else
               RawIndex_Int := StringToInteger.From_String (RawIndex_Str);

               if RawIndex_Int < 1 or else RawIndex_Int > 255 then
                  Put_Line ("Index out of range.");
               else
                  declare
                     Loc : MemoryStore.Location_Index :=
                       MemoryStore.Location_Index (RawIndex_Int);
                  begin
                     MemoryStore.Remove (Mem, Loc);
                     Put_Line
                       ("Removed location " & Integer'Image (RawIndex_Int));
                  end;
               end if;
            end if;
         end;
      end if;
   end Handle_Remove;

   procedure Handle_LoadFrom
     (S         : in Lines.MyString;
      T         : in MyStringTokeniser.TokenArray;
      Stack     : in out Operand_Stack;
      Stack_Top : in out Natural;
      Unlocked  : in Boolean;
      NumTokens : in Natural;
      Mem       : in MemoryStore.Database)
   is
      RawIndex_Int : Integer;
   begin
      if NumTokens /= 2 or else T'First > 2 or else T'Last < 2 then
         Put_Line ("Usage: loadfrom <index>");
      elsif T (2).Start < 1 or else T (2).Length < 1 then
         Put_Line ("Invalid token.");
      elsif Stack_Top >= Max_Stack_Size then
         Put_Line ("Error: stack overflow.");
      else
         declare
            RawIndex_Str : constant String := Get_Token_String (S, T (2));
         begin
            if RawIndex_Str'Length = 0 then
               Put_Line ("Invalid index.");
            else
               RawIndex_Int := StringToInteger.From_String (RawIndex_Str);
               if RawIndex_Int < 1 or else RawIndex_Int > 255 then
                  Put_Line ("Index out of range.");
               else
                  declare
                     Loc : MemoryStore.Location_Index :=
                       MemoryStore.Location_Index (RawIndex_Int);
                  begin
                     if not MemoryStore.Has (Mem, Loc) then
                        Put_Line ("Error: no value at that location.");
                     else
                        declare
                           Val : Integer :=
                             Integer (MemoryStore.Get (Mem, Loc));
                        begin
                           Stack_Top := Stack_Top + 1;
                           Stack (Stack_Top) := Val;

                           Put_Line
                             ("Loaded "
                              & Integer'Image (Val)
                              & " from location "
                              & Integer'Image (RawIndex_Int));
                        end;
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
   end Handle_LoadFrom;

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

   function Trim_Trailing_Spaces (Input : in String) return String is
      Last_Non_Space : Integer := Input'First - 1;
   begin
      for I in reverse Input'Range loop
         if Input (I) /= ' ' then
            Last_Non_Space := I;
            exit;
         end if;
      end loop;

      if Last_Non_Space < Input'First then
         return "";  -- Entire string is spaces

      else
         return Input (Input'First .. Last_Non_Space);
      end if;
   end Trim_Trailing_Spaces;

end CalculatorCommands;
