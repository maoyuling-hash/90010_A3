pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;

with CalculatorCommands;

procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S : Lines.MyString;

   -- 分词数组
   T : MyStringTokeniser.TokenArray (1 .. 5);

   NumTokens : Natural;

   -- 存储与 PIN 状态
   Mem       : MemoryStore.Database;
   MasterPIN : PIN.PIN;
   Unlocked  : Boolean := False;

   -- 操作数栈定义
   Max_Stack_Size : constant := 512;
   type Operand_Stack is array (1 .. Max_Stack_Size) of Integer;
   Stack          : Operand_Stack;

   Stack_Top : Natural := 0;

   -- 安全获取 token 子串（带边界和溢出检查）
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

   -- 判断字符串是否是合法 PIN
   function Is_Valid_PIN_String (S : String) return Boolean is
   begin
      return S'Length = 4 and then (for all C of S => C in '0' .. '9');
   end Is_Valid_PIN_String;

begin
   -- 启动时检查是否提供主密码
   if MyCommandLine.Argument_Count < 1 then
      Put_Line ("Error: No master PIN provided.");
      return;
   end if;

   -- 设置主密码
   declare
      Arg : constant String := MyCommandLine.Argument (1);
   begin
      if Is_Valid_PIN_String (Arg) then
         MasterPIN := PIN.From_String (Arg);
      else
         Put_Line ("Invalid master PIN: must be 4-digit numeric string.");
         return;
      end if;
   end;

   -- 初始化内存、主循环
   MemoryStore.Init (Mem);
   loop
      if Unlocked then
         Put ("unlocked> ");
      else
         Put ("locked> ");
      end if;

      Lines.Get_Line (S);
      if Lines.Length (S) > 2048 then
         Put_Line ("Error: Input too long.");
         return;
      end if;

      T := (others => (Start => 1, Length => 0));
      MyStringTokeniser.Tokenise (Lines.To_String (S), T, NumTokens);
      if T (1).Length > 0 or NumTokens /= 0 then
         declare
            Cmd : constant String := Get_Token_String (S, T (1));
         begin
            if Cmd'Length > 20 then
               Put_Line ("Command too long.");
            elsif Cmd = "unlock" then
               if T (2).Length = 0 then
                  Put_Line ("Usage: unlock <pin>");
               else
                  declare
                     RawPIN : constant String := Get_Token_String (S, T (2));
                  begin
                     if Is_Valid_PIN_String (RawPIN) then
                        CalculatorCommands.Handle_Unlock
                          (MasterPIN, PIN.From_String (RawPIN), Unlocked);
                     else
                        Put_Line
                          ("Invalid PIN: must be a 4-digit numeric string.");
                     end if;
                  end;
               end if;

            elsif Cmd = "lock" then
               if T (2).Length = 0 then
                  Put_Line ("Usage: lock <newpin>");
               else
                  declare
                     RawPIN : constant String := Get_Token_String (S, T (2));
                  begin
                     if Is_Valid_PIN_String (RawPIN) then
                        declare
                           NewPIN : PIN.PIN := PIN.From_String (RawPIN);
                        begin
                           CalculatorCommands.Handle_Lock
                             (NewPIN, MasterPIN, Unlocked);
                        end;
                     else
                        Put_Line
                          ("Invalid PIN: must be a 4-digit numeric string.");
                     end if;
                  end;
               end if;
            elsif T (1).Length > 0 and then Cmd = "push1" then
               if not Unlocked then
                  Put_Line ("Command not allowed: calculator is locked.");
               elsif T (2).Length = 0 then
                  Put_Line ("Usage: push1 <number>");
               elsif Stack_Top >= Max_Stack_Size then
                  Put_Line ("Error: stack overflow.");
               else
                  declare
                     RawVal : String := Get_Token_String (S, T (2));
                  begin
                     declare
                        Val : constant Integer :=
                          StringToInteger.From_String (RawVal);
                     begin
                        Stack_Top := Stack_Top + 1;
                        Stack (Stack_Top) := Val;
                        Put_Line
                          ("Pushed "
                           & Integer'Image (Val)
                           & " onto the stack.");
                     end;
                  end;
               end if;

            else
               Put_Line ("Unknown command: " & Cmd);
            end if;
         end;
      else
         Put_Line ("No command entered.");
      end if;
   end loop;
end Main;
