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

procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   S : Lines.MyString;

   -- 分词数组
   T : MyStringTokeniser.TokenArray (1 .. 5) :=
     (others => (Start => 1, Length => 0));

   NumTokens : Natural;

   -- 存储与 PIN 状态
   Mem        : MemoryStore.Database;
   MasterPIN  : PIN.PIN;
   CurrentPIN : PIN.PIN;
   Unlocked   : Boolean := False;

   function Get_Token_String
     (S : Lines.MyString; Tok : MyStringTokeniser.TokenExtent) return String is
   begin
      return
        Lines.To_String
          (Lines.Substring (S, Tok.Start, Tok.Start + Tok.Length - 1));
   end Get_Token_String;

begin
   -- 启动的时候必须输入指定密码
   if MyCommandLine.Argument_Count < 1 then
      Put_Line ("Error: No master PIN provided.");
      return;
   end if;

   -- 设置主密码
   declare
      Arg : constant String := MyCommandLine.Argument (1);
   begin
      MasterPIN := PIN.From_String (Arg);
   end;

   ------------------------------------------------------------------
   -- Step 2: 初始化内存、主循环
   ------------------------------------------------------------------
   MemoryStore.Init (Mem);
   loop
      -- 显示提示符
      if Unlocked then
         Put ("unlocked> ");
      else
         Put ("locked> ");
      end if;

      -- 读一行输入
      Lines.Get_Line (S);

      -- 检查长度
      if Lines.Length (S) > 2048 then
         Put_Line ("Error: Input too long.");
         return;
      end if;

      -- 分词
      MyStringTokeniser.Tokenise (Lines.To_String (S), T, NumTokens);
      if T (1).Length > 0 then
         declare
            Cmd : constant String := Get_Token_String (S, T (1));
         begin
            if Cmd = "unlock" then
               if Unlocked then
                  Put_Line ("Already unlocked.");
               else
                  if T (2).Length = 0 then
                     Put_Line ("Usage: unlock <pin>");
                  else
                     CurrentPIN :=
                       PIN.From_String (Get_Token_String (S, T (2)));
                     if PIN."=" (CurrentPIN, MasterPIN) then
                        Unlocked := True;
                     else
                        Put_Line ("Incorrect PIN. Still locked.");
                     end if;
                  end if;
               end if;
            end if;
         end;
      else
         Put_Line ("No command entered.");
      end if;
   end loop;
end Main;
