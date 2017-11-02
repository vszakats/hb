Harbour Shell / Script Runner 3.4.0dev \(c390da90ad\) \(2017-10-10 16:11\)  
Copyright &copy; 2007-present, Viktor Szakats  
Copyright &copy; 2003-2007, Przemysław Czerpak  
<https://github.com/vszakats/harbour-core/>  
Μετάφραση \(el\): Pete D. \(pete\_westg@yahoo.gr\)  

Σύνταξη:  
  
  hbrun &lt;file\[.hb|.prg|.hrb|.dbf\]|-dbf:file|-prg:string&gt;|&lt;option&gt; \[&lt;παραμετρος\[ι\]&gt;\]  
  
Περιγραφή:  


  Το hbrun μπορεί να εκτελεί σενάρια Harbour \(πηγαία ή προκατασκευασμένα\), ενώ προσφέρει επίσης ένα διαδραστικό περιβάλλον εργασίας.
  
Οι παρακάτω επιλογές είναι διαθέσιμες στη γραμμή-εντολών:  


 - **--hb:debug** Ενεργοποίηση αποσφαλμάτωσης script


 - **-help** η παρούσα βοήθεια
 - **-viewhelp** full help in text viewer
 - **-fullhelp** full help
 - **-fullhelpmd** full help in [Markdown](https://daringfireball.net/projects/markdown/) format
 - **-version** εμφάνιση κεφαλίδας έκδοσης μόνο
  
Αρχεία:  


 - **\*.hb** Σενάριο Harbour
 - **\*.hrb** Μεταφέρσιμο δυαδικό Harbour \(γνωστό και ως προκατασκευασμένο σενάριο\)
 - **hbstart.hb** εναρκτήριο σενάριο Harbour για το διαδραστικό κέλυφος Harbour. Εκτελείται αυτόματα κατά την έναρξη του κελύφους, αν υπάρχει. Ενδεχόμενες τοποθεσίες \(με σειρά προτεραιότητας\) \[\*\]: ./, $HOME/.harbour, /etc/harbour, etc/harbour, etc, &lt;hbrun κατάλογος&gt;
 - **shell plugins** .hb και .hrb plugins για το διαδραστικό κέλυφος του Harbour. Πρέπει να βρίσκονται μέσα στο \[\*\]: $HOME/.harbour/
 - **.hb\_history** αποθηκεύει ιστορικό εντολών για το διαδραστικό κέλυφος του Harbour. Μπορείτε να απενεργοποιήσετε το ιστορικό κάνοντας την πρώτη γραμμή 'no' \(χωρίς τα εισαγωγικά και με νεα γραμμή\). Βρίσκεται στο \[\*\]: $HOME/.harbour/
 - **hb\_extension** λίστα καταλήξεων προς φόρτωση στο διαδραστικό κέλυφος του Harbour. Μία κατάληξη ανα γραμμή, το τμήμα της γραμμής μετά από ένα χαρακτήρα '\#' αγνοείται. Εναλλακτικά ονομα-αρχείου στο Ms-DOS: Το hb\_ext.ini. Βρίσκεται μεσα στο \[\*\]: $HOME/.harbour/


Predefined constants in sources \(do not define them manually\):


 - **\_\_HBSCRIPT\_\_HBSHELL** όταν ένα πηγαίο αρχείο Harbour εκτελείται ως σενάριο κελύφους
 - **&lt;standard Harbour&gt;** \_\_PLATFORM\_\_\*, \_\_ARCH\*BIT\_\_, \_\_\*\_ENDIAN\_\_, etc.
  
Μεταβλητές περιβάλλοντος:  


 - **HB\_EXTENSION** λίστα καταλήξεων, διαχωρισμένων με κενό διάστημα, προς φόρτωση στο διαδραστικό κέλυφος του Harbour
  
Shell API διαθέσιμο σε σενάρια Harbour:  


 - **hbshell\_gtSelect\( \[&lt;cGT&gt;\] \) -&gt; NIL**  
Αλλαγή GT. Προεπιλογή \[\*\]: 'gttrm'
 - **hbshell\_Clipper\(\) -&gt; NIL**  
Enable Cl\*pper compatibility \(non-Unicode\) mode.
 - **hbshell\_include\( &lt;cHeader&gt; \) -&gt; &lt;lSuccess&gt;**  
Φόρτωση Harbour header.
 - **hbshell\_uninclude\( &lt;cHeader&gt; \) -&gt; &lt;lSuccess&gt;**  
Αποφόρτωση Harbour header.
 - **hbshell\_include\_list\(\) -&gt; NIL**  
Εμφάνιση λίστας των φορτωμένων Harbour header.
 - **hbshell\_ext\_load\( &lt;cPackageName&gt; \) -&gt; &lt;lSuccess&gt;**  
Φόρτωση πακέτου. Παρόμοιο με τη ντιρεκτίβα \#request PP.
 - **hbshell\_ext\_unload\( &lt;cPackageName&gt; \) -&gt; &lt;lSuccess&gt;**  
Αποφόρτωση πακέτου
 - **hbshell\_ext\_get\_list\(\) -&gt; &lt;aPackages&gt;**  
Λίστα φορτωμένων πακέτων
 - **hbshell\_DirBase\(\) -&gt; &lt;cBaseDir&gt;**  
Το hb\_DirBase\(\) δεν χαρτογραφήθηκε σε σενάριο.
 - **hbshell\_ProgName\(\) -&gt; &lt;cPath&gt;**  
Το hb\_ProgName\(\) δεν χαρτογραφήθηκε σε σενάριο.
 - **hbshell\_ScriptName\(\) -&gt; &lt;cPath&gt;**  
Name of the script executing.
  
Σημειώσεις:  


  - .hb, .prg, .hrb ή .dbf file passed as first parameter will be run as Harbour script. If the filename contains no path components, it will be searched in current working directory and in PATH. If not extension is given, .hb and .hrb extensions are searched, in that order. .dbf file will be opened automatically in shared mode and interactive Harbour shell launched. .dbf files with non-standard extension can be opened by prepending '-dbf:' to the file name. Otherwise, non-standard extensions will be auto-detected for source and precompiled script types. Note, for Harbour scripts, the codepage is set to UTF-8 by default. The default core header 'hb.ch' is automatically \#included at the interactive shell prompt. The default date format is the ISO standard: yyyy-mm-dd. SET EXACT is set to ON. Set\( \_SET\_EOL \) is set to OFF. The default GT is 'gtcgi', unless full-screen CUI calls are detected, when 'gttrm' \[\*\] is automatically selected \(except for INIT PROCEDUREs\).
  - You can use key &lt;Ctrl\+V&gt; in interactive Harbour shell to paste text from the clipboard.
  - Τιμές με αστερίσκο \[\*\] μπορεί να εξαρτώνται από την πλατφόρμα υποδοχής ή/και τη διαμόρφωση. Η παρούσα βοήθεια δημιουργήθηκε στην 'darwin' πλατφόρμα υποδοχής.
  
Αδεια:  


  This program is free software; you can redistribute it and/or modify  
it under the terms of the GNU General Public License as published by  
the Free Software Foundation; either version 2 of the License, or  
\(at your option\) any later version.  
  
This program is distributed in the hope that it will be useful,  
but WITHOUT ANY WARRANTY; without even the implied warranty of  
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the  
GNU General Public License for more details.  
  
You should have received a copy of the GNU General Public License  
along with this program; if not, write to the Free Software Foundation,  
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  
\(or visit their website at https://www.gnu.org/licenses/\).  
  
License extensions:  
  - This source code must be kept and distributed as part  
    of the Harbour package and/or the placement of the tool sources  
    and files must reflect that it is part of Harbour Project.  
  - Copyright information must always be presented by  
    projects including this tool or help text.  
  - Modified versions of the tool must clearly state this  
    fact on the copyright screen.  
  - Source code modifications shall always be made available  
    along with binaries.  
  - Help text and documentation is licensed under  
    Creative Commons Attribution-ShareAlike 4.0 International:  
    https://creativecommons.org/licenses/by-sa/4.0/  

  
Συγγραφέας:  


 - Viktor Szakats \(vszakats.net/harbour\) 
