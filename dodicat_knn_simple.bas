
#INCLUDE "file.bi"

Function Splitstring(s_in As String,chars As String,result() As String) As Long
    Dim As Long ctr,ctr2,k,n,LC=Len(chars)
    Dim As boolean tally(Len(s_in))
    #macro check_instring()
    n=0
    While n<Lc
        If chars[n]=s_in[k] Then
            tally(k)=true
            If (ctr2-1) Then ctr+=1
            ctr2=0
            Exit While
        End If
        n+=1
    Wend
    #endmacro
   
    #macro splice()
    If tally(k) Then
        If (ctr2-1) Then ctr+=1:result(ctr)=Mid(s_in,k+2-ctr2,ctr2-1)
        ctr2=0
    End If
    #endmacro
    '==================  LOOP TWICE =======================
    For k  =0 To Len(s_in)-1
        ctr2+=1:check_instring()
    Next k
    If ctr=0 Then
        If Len(s_in) Andalso Instr(chars,Chr(s_in[0])) Then ctr=1':beep
    End If
    If ctr Then Redim result(1 To ctr): ctr=0:ctr2=0 Else  Return 0
    For k  =0 To Len(s_in)-1
        ctr2+=1:splice()
    Next k
    '===================== Last one ========================
    If ctr2>0 Then
        Redim Preserve result(1 To ctr+1)
        result(ctr+1)=Mid(s_in,k+1-ctr2,ctr2)
    End If
   
    Return Ubound(result)
End Function

Function loadfiletostring(file As String) As String
    Var  f=Freefile
    Open file For Binary Access Read As #f
    Dim As String text
    If Lof(f) > 0 Then
        text = String(Lof(f), 0)
        Get #f, , text
    End If
    Close #f
    Return text
End Function

Function vdist(p1 As String,p2 As String,col As Long) As Double
    Dim As Double acc
    Redim As String t1(),t2()
    splitstring(p1,",",t1())
    splitstring(p2,",",t2())
    For n As Long=1 To col
        acc+=(Val(t1(n))-Val(t2(n)))^2
    Next
    Return Sqr(acc)
End Function

Function GetClosest(a() As String,ans() As Long,v As String,num As Long,col As Long) As Long
    Dim As Double d,i
    Dim As Long ctr
    Dim As Long done(Lbound(a) To Ubound(a))
    Do
        d=2e8
        For n As Long=Lbound(a) To Ubound(a)
            Var dst=vdist(a(n),v,col)
            If d>dst And done(n)=0 Then d=dst:i=n:done(n)=1
        Next n
        ctr+=1
        Redim Preserve ans(1 To ctr)
        ans(ctr)=i
    Loop Until Ubound(ans)>=num
    Return Ubound(ans)
End Function

Sub knn_main(f As String)
    Dim As Long col
   
    Dim As String s=loadfiletostring(f)'("knn-dataset-plant.csv")
    Redim As String g()
    Redim As String tmp()
   
    splitstring(s,Chr(10),g()) 'load the file into a string array g()
   
    'test to get col
    splitstring(g(1),",",tmp())
    col=Ubound(tmp)-1  'get the non strings parts of the string here (col)
    Print "The file:"
    For n As Long=Lbound(g) To Ubound(g)
        Print n,g(n)
    Next n
   
    Print
   
    Redim As Long near()
   
    GetClosest(g(),near(),g(10),5,col)'' <<--------  here, get 3 closest to array(10)
    Print "This is a fixed test for 5 (closest to 10 and including 10): using Getclosest()"
   
    Print "index",,"values"
    For n As Long=Lbound(near) To Ubound(near)
        Print near(n),
        Print g(near(n))
    Next
   
   
    '============= new predictions!!!=================
    Dim answer As String
    Do
        Dim predict As String
        Dim As Double x1
        Dim neighbors As Long
        Print
        Dim As String s
        Do
            Print "press 1 for closest to a flower"
            Print "press 2 to enter predicted values"
            s=Input(1)
        Loop Until s="1" Or s="2"
        If s="1" Then
            Do
                Print "Between ";Lbound(g);" and ";Ubound(g)
                Input "insert index : "; x1
            Loop Until x1>=Lbound(g) And x1<=Ubound(g)
        End If
        If s="2" Then
            Dim As Double v
            Dim As String s
            For n As Long=1 To col
                Print n; " of ";col;",   enter a value ";
                Input; v
                s+=Str(v)+","
                Print
            Next n
            s=Rtrim(s,",")
            predict=s
            print, predict
        End If
       
        Input "how many neighbors TO search?: "; neighbors
       
        Redim As Long near()
        If s="1" Then GetClosest(g(),near(),g(x1),neighbors,col)   'using an existing array element
        If s="2" Then GetClosest(g(),near(),predict,neighbors,col) ' <- new predictions for new values
        Print "using Getclosest()"
       
        Print "index",,"values"
        For n As Long=Lbound(near) To Ubound(near)
            Print near(n),
            Print g(near(n))
        Next
        Print "another prediction? y/n: "
        answer=Input(1)
       
    Loop Until answer = "n"
End Sub

'========== RUN ==========
KNN_MAIN("\iris.csv")

Sleep()
 