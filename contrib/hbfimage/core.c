/*
 * FreeImage graphic library low level (client api) interface code.
 *
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* TOFIX: To use strings (or other solutions) for remaining raw pointers. */

#include "hbfiint.h"

#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"

#define hb_fi_retl( x )  hb_retl( ( x ) ? HB_TRUE : HB_FALSE )
#define hb_fi_parl( x )  ( hb_parl( x ) ? TRUE : FALSE )

/* Error callback */

typedef struct
{
   PHB_ITEM pErrorCallback;
} HB_FI_ERROR;

static void hb_fi_error_init( void * cargo )
{
   HB_FI_ERROR * pError = ( HB_FI_ERROR * ) cargo;

   pError->pErrorCallback = NULL;
}

static void hb_fi_error_release( void * cargo )
{
   HB_FI_ERROR * pError = ( HB_FI_ERROR * ) cargo;

   if( pError->pErrorCallback )
      hb_itemRelease( pError->pErrorCallback );
}

static HB_TSD_NEW( s_fi_error,
                   sizeof( HB_FI_ERROR ),
                   hb_fi_error_init,
                   hb_fi_error_release );

/* HB_FIBITMAP API */

typedef struct
{
   FIBITMAP * dib;
   HB_BOOL    fFree;
} HB_FIBITMAP, * PHB_FIBITMAP;

static PHB_FIBITMAP PHB_FIBITMAP_create( FIBITMAP * dib, HB_BOOL fFree )
{
   PHB_FIBITMAP hb_dib = ( PHB_FIBITMAP ) hb_xgrab( sizeof( HB_FIBITMAP ) );

   hb_dib->dib   = dib;
   hb_dib->fFree = fFree;

   return hb_dib;
}

static void PHB_FIBITMAP_free( PHB_FIBITMAP hb_dib )
{
   if( hb_dib->fFree )
      FreeImage_Unload( hb_dib->dib );

   hb_xfree( hb_dib );
}

/* HB_FIBITMAP GC handler */

/* FIBITMAP destructor, it's executed automatically */
static HB_GARBAGE_FUNC( hb_FIBITMAP_Destructor )
{
   /* Retrieve image pointer holder */
   HB_FIBITMAP ** ptr = ( HB_FIBITMAP ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
   {
      PHB_FIBITMAP_free( *ptr );

      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcFIBITMAPFuncs =
{
   hb_FIBITMAP_Destructor,
   hb_gcDummyMark
};

FIBITMAP * hb_FIBITMAP_par( int iParam )
{
   HB_FIBITMAP ** ptr = ( HB_FIBITMAP ** ) hb_parptrGC( &s_gcFIBITMAPFuncs, iParam );

   return ptr ? ( *ptr )->dib : NULL;
}

void hb_FIBITMAP_ret( FIBITMAP * dib, HB_BOOL fFree )
{
   if( dib )
   {
      HB_FIBITMAP ** ptr = ( HB_FIBITMAP ** ) hb_gcAllocate( sizeof( HB_FIBITMAP * ),
                                                             &s_gcFIBITMAPFuncs );

      *ptr = PHB_FIBITMAP_create( dib, fFree );

      hb_retptrGC( ( void * ) ptr );
   }
   else
      hb_retptr( NULL );
}

/* FIMULTIBITMAP GC handler */

/* FIMULTIBITMAP destructor, it's executed automatically */
static HB_GARBAGE_FUNC( hb_FIMULTIBITMAP_Destructor )
{
   /* Retrieve image pointer holder */
   FIMULTIBITMAP ** ptr = ( FIMULTIBITMAP ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ptr )
      /* set pointer to NULL to avoid multiple freeing */
      *ptr = NULL;
}

static const HB_GC_FUNCS s_gcFIMULTIBITMAPFuncs =
{
   hb_FIMULTIBITMAP_Destructor,
   hb_gcDummyMark
};

static FIMULTIBITMAP * hb_FIMULTIBITMAP_par( int iParam )
{
   FIMULTIBITMAP ** ptr = ( FIMULTIBITMAP ** ) hb_parptrGC( &s_gcFIMULTIBITMAPFuncs, iParam );

   return ptr ? *ptr : NULL;
}

static void hb_FIMULTIBITMAP_ret( FIMULTIBITMAP * bitmap )
{
   if( bitmap )
   {
      FIMULTIBITMAP ** ptr = ( FIMULTIBITMAP ** ) hb_gcAllocate( sizeof( FIMULTIBITMAP * ),
                                                                 &s_gcFIMULTIBITMAPFuncs );

      *ptr = bitmap;

      hb_retptrGC( ( void * ) ptr );
   }
   else
      hb_retptr( NULL );
}

/* *** WRAPPED FUNCTIONS *** */

/* Init / Error routines */

HB_FUNC( FI_INITIALISE )
{
   FreeImage_Initialise( hb_fi_parl( 1 ) /* load_local_plugins_only */ );
}

HB_FUNC( FI_DEINITIALISE )
{
   FreeImage_DeInitialise();
}

/* Version routines */

HB_FUNC( FI_GETVERSION )
{
   hb_retc( FreeImage_GetVersion() );
}

HB_FUNC( FI_GETCOPYRIGHTMESSAGE )
{
   hb_retc( FreeImage_GetCopyrightMessage() );
}

/* Message output functions */

/* DLL_API void DLL_CALLCONV FreeImage_OutputMessageProc(int fif, const char *fmt, ...); */

/* typedef void (*FreeImage_OutputMessageFunction)(FREE_IMAGE_FORMAT fif, const char *msg); */

static void FreeImageErrorHandler( FREE_IMAGE_FORMAT fif, const char * message )
{
   HB_FI_ERROR * pError = ( HB_FI_ERROR * ) hb_stackGetTSD( &s_fi_error );

   if( pError )
   {
      PHB_ITEM pErrorCallback = pError->pErrorCallback;

      if( pErrorCallback && hb_vmRequestReenter() )
      {
         const char * format = FreeImage_GetFormatFromFIF( fif );

         hb_vmPushEvalSym();
         hb_vmPush( pErrorCallback );
         hb_vmPushString( format, strlen( format ) );
         hb_vmPushString( message, strlen( message ) );
         hb_vmSend( 2 );

         hb_vmRequestRestore();
      }
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_SetOutputMessage(FreeImage_OutputMessageFunction omf); */
/* implementation: void FreeImage_SetOutputMessage( pFunctionPointer ) */
HB_FUNC( FI_SETOUTPUTMESSAGE )
{
   if( HB_ISEVALITEM( 1 ) )
   {
      HB_FI_ERROR * pError = ( HB_FI_ERROR * ) hb_stackGetTSD( &s_fi_error );

      if( pError->pErrorCallback )
         hb_itemRelease( pError->pErrorCallback );
      pError->pErrorCallback = hb_itemNew( hb_param( 1, HB_IT_EVALITEM ) );

      FreeImage_SetOutputMessage( FreeImageErrorHandler );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Allocate / Clone / Unload routines */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Allocate(int width, int height, int bpp, unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0)); */
HB_FUNC( FI_ALLOCATE )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      hb_FIBITMAP_ret( FreeImage_Allocate( hb_parni( 1 ) /* width */,
                                           hb_parni( 2 ) /* height */,
                                           hb_parni( 3 ) /* bpp */,
                                           ( unsigned ) hb_parni( 4 ) /* red_mask */,
                                           ( unsigned ) hb_parni( 5 ) /* green_mask */,
                                           ( unsigned ) hb_parni( 6 ) /* blue_mask */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_AllocateT(FREE_IMAGE_TYPE type, int width, int height, int bpp FI_DEFAULT(8), unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0)); */
HB_FUNC( FI_ALLOCATET )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      hb_FIBITMAP_ret( FreeImage_AllocateT( ( FREE_IMAGE_TYPE ) hb_parni( 1 ) /* type */,
                                            hb_parni( 2 ) /* width */,
                                            hb_parni( 3 ) /* height */,
                                            hb_parni( 3 ) /* bpp */,
                                            ( unsigned ) hb_parni( 4 ) /* red_mask */,
                                            ( unsigned ) hb_parni( 5 ) /* green_mask */,
                                            ( unsigned ) hb_parni( 6 ) /* blue_mask */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP * DLL_CALLCONV FreeImage_Clone(FIBITMAP *dib); */
HB_FUNC( FI_CLONE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_Clone( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if defined( HB_LEGACY_LEVEL4 )

HB_FUNC( FI_UNLOAD )
{
}

#endif

/* Load / Save routines */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromMemory(FREE_IMAGE_FORMAT fif, FIMEMORY *stream, int flags FI_DEFAULT(0)); */
/* DLL_API FIMEMORY *DLL_CALLCONV FreeImage_OpenMemory(BYTE *data FI_DEFAULT(0), DWORD size_in_bytes FI_DEFAULT(0)); */
/* DLL_API void DLL_CALLCONV FreeImage_CloseMemory(FIMEMORY *stream); */
HB_FUNC( FI_LOADFROMMEMORY )
{
   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && HB_ISNUM( 3 ) )
   {
      FIMEMORY * stream = FreeImage_OpenMemory( ( BYTE * ) HB_UNCONST( hb_parc( 2 ) /* image */ ), ( DWORD ) hb_parclen( 2 ) );

      hb_FIBITMAP_ret( FreeImage_LoadFromMemory( ( FREE_IMAGE_FORMAT ) hb_parni( 1 ),
                                                 stream,
                                                 hb_parni( 3 ) /* flags */ ), HB_TRUE );

      FreeImage_CloseMemory( stream );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Load(FREE_IMAGE_FORMAT fif, const char *filename, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_LOAD )
{
   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && HB_ISNUM( 3 ) )
      hb_FIBITMAP_ret( FreeImage_Load( ( FREE_IMAGE_FORMAT ) hb_parni( 1 ),
                                       hb_parc( 2 ) /* filename */,
                                       hb_parni( 3 ) /* flags */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadU(FREE_IMAGE_FORMAT fif, const wchar_t *filename, int flags FI_DEFAULT(0)); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromHandle(FREE_IMAGE_FORMAT fif, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0)); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_Save(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_SAVE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 2 );

   if( HB_ISNUM( 1 ) && dib && HB_ISCHAR( 3 ) && HB_ISNUM( 4 ) )
      hb_fi_retl( FreeImage_Save( ( FREE_IMAGE_FORMAT ) hb_parni( 1 ),
                                  dib,
                                  hb_parc( 3 ) /* filename */,
                                  hb_parni( 4 ) /* flags */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SaveU(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const wchar_t *filename, int flags FI_DEFAULT(0)); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SaveToHandle(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0)); */

/* Memory I/O stream routines */

/*
   DLL_API FIMEMORY *DLL_CALLCONV FreeImage_OpenMemory(BYTE *data FI_DEFAULT(0), DWORD size_in_bytes FI_DEFAULT(0));
   DLL_API void DLL_CALLCONV FreeImage_CloseMemory(FIMEMORY *stream);
   DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromMemory(FREE_IMAGE_FORMAT fif, FIMEMORY *stream, int flags FI_DEFAULT(0));
   DLL_API BOOL DLL_CALLCONV FreeImage_SaveToMemory(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FIMEMORY *stream, int flags FI_DEFAULT(0));
   DLL_API long DLL_CALLCONV FreeImage_TellMemory(FIMEMORY *stream);
   DLL_API BOOL DLL_CALLCONV FreeImage_SeekMemory(FIMEMORY *stream, long offset, int origin);
   DLL_API BOOL DLL_CALLCONV FreeImage_AcquireMemory(FIMEMORY *stream, BYTE **data, DWORD *size_in_bytes);
 */

/* Plugin Interface */

/*
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterLocalPlugin(FI_InitProc proc_address, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterExternalPlugin(const char *path, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
   DLL_API int DLL_CALLCONV FreeImage_GetFIFCount(void);
   DLL_API int DLL_CALLCONV FreeImage_SetPluginEnabled(FREE_IMAGE_FORMAT fif, BOOL enable);
   DLL_API int DLL_CALLCONV FreeImage_IsPluginEnabled(FREE_IMAGE_FORMAT fif);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFormat(const char *format);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromMime(const char *mime);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFormatFromFIF(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFExtensionList(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFDescription(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFRegExpr(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFMimeType(FREE_IMAGE_FORMAT fif);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilename(const char *filename);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilenameU(const wchar_t *filename);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsReading(FREE_IMAGE_FORMAT fif);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsWriting(FREE_IMAGE_FORMAT fif);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportBPP(FREE_IMAGE_FORMAT fif, int bpp);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportType(FREE_IMAGE_FORMAT fif, FREE_IMAGE_TYPE type);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsICCProfiles(FREE_IMAGE_FORMAT fif);
 */

/* Multipaging interface */

/* DLL_API FIMULTIBITMAP * DLL_CALLCONV FreeImage_OpenMultiBitmap(FREE_IMAGE_FORMAT fif, const char *filename, BOOL create_new, BOOL read_only, BOOL keep_cache_in_memory FI_DEFAULT(FALSE), int flags FI_DEFAULT(0)); */
HB_FUNC( FI_OPENMULTIBITMAP )
{
   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && HB_ISLOG( 3 ) && HB_ISLOG( 4 ) )
      hb_FIMULTIBITMAP_ret( FreeImage_OpenMultiBitmap( ( FREE_IMAGE_FORMAT ) hb_parni( 1 ) /* fif */,
                                                       hb_parc( 2 ) /* filename */,
                                                       hb_fi_parl( 3 ) /* create_new */,
                                                       hb_fi_parl( 4 ) /* read_only */,
                                                       hb_fi_parl( 5 ) /* keep_cache_in_memory */,
                                                       hb_parni( 6 ) /* flags */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_CloseMultiBitmap(FIMULTIBITMAP *bitmap, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_CLOSEMULTIBITMAP )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );

   if( bitmap )
      hb_fi_retl( FreeImage_CloseMultiBitmap( bitmap,
                                              hb_parni( 2 ) /* flags */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API int DLL_CALLCONV FreeImage_GetPageCount(FIMULTIBITMAP *bitmap); */
HB_FUNC( FI_GETPAGECOUNT )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );

   if( bitmap )
      hb_retni( FreeImage_GetPageCount( bitmap ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_AppendPage(FIMULTIBITMAP *bitmap, FIBITMAP *data); */
HB_FUNC( FI_APPENDPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );
   FIBITMAP *      data   = hb_FIBITMAP_par( 2 );

   if( bitmap && data )
      FreeImage_AppendPage( bitmap, data );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_InsertPage(FIMULTIBITMAP *bitmap, int page, FIBITMAP *data); */
HB_FUNC( FI_INSERTPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );
   FIBITMAP *      data   = hb_FIBITMAP_par( 3 );

   if( bitmap && HB_ISNUM( 2 ) && data )
      FreeImage_InsertPage( bitmap,
                            hb_parni( 2 ) - 1 /* page, 0-based index */,
                            data );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_DeletePage(FIMULTIBITMAP *bitmap, int page); */
HB_FUNC( FI_DELETEPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );

   if( bitmap && HB_ISNUM( 2 ) )
      FreeImage_DeletePage( bitmap, hb_parni( 2 ) - 1 /* page, 0-based index */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP * DLL_CALLCONV FreeImage_LockPage(FIMULTIBITMAP *bitmap, int page); */
HB_FUNC( FI_LOCKPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );

   if( bitmap && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_LockPage( bitmap, hb_parni( 2 ) - 1 /* page, 0-based index */ ), HB_FALSE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_UnlockPage(FIMULTIBITMAP *bitmap, FIBITMAP *page, BOOL changed); */
HB_FUNC( FI_UNLOCKPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );
   FIBITMAP *      page   = hb_FIBITMAP_par( 2 );

   if( bitmap && page && HB_ISLOG( 3 ) )
      FreeImage_UnlockPage( bitmap, page, hb_fi_parl( 3 ) /* changed */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_MovePage(FIMULTIBITMAP *bitmap, int target, int source); */
HB_FUNC( FI_MOVEPAGE )
{
   FIMULTIBITMAP * bitmap = hb_FIMULTIBITMAP_par( 1 );

   if( bitmap && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
      hb_fi_retl( FreeImage_MovePage( bitmap,
                                      hb_parni( 2 ) /* target */,
                                      hb_parni( 3 ) /* source */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetLockedPageNumbers(FIMULTIBITMAP *bitmap, int *pages, int *count); */

/* Filetype request routines */

/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileType(const char *filename, int size FI_DEFAULT(0)); */
HB_FUNC( FI_GETFILETYPE )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( FreeImage_GetFileType( hb_parc( 1 ) /* filename */,
                                       ( int ) hb_parclen( 1 ) /* size */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeU(const wchar_t *filename, int size FI_DEFAULT(0)); */
/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromHandle(FreeImageIO *io, fi_handle handle, int size FI_DEFAULT(0)); */
/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromMemory(FIMEMORY *stream, int size FI_DEFAULT(0)); */
HB_FUNC( FI_GETFILETYPEFROMMEMORY )
{
   if( HB_ISCHAR( 1 ) )
   {
      FIMEMORY * stream = FreeImage_OpenMemory( ( BYTE * ) HB_UNCONST( hb_parc( 1 ) ), ( int ) hb_parclen( 1 ) );

      hb_retni( FreeImage_GetFileTypeFromMemory( stream, hb_parni( 1 ) /* size */ ) );

      FreeImage_CloseMemory( stream );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Image type request routine */

HB_FUNC( FI_GETIMAGETYPE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetImageType( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* FreeImage helper routines */

/* DLL_API BOOL DLL_CALLCONV FreeImage_IsLittleEndian(void); */
HB_FUNC( FI_ISLITTLEENDIAN )
{
   hb_fi_retl( FreeImage_IsLittleEndian() );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_LookupX11Color(const char *szColor, BYTE *nRed, BYTE *nGreen, BYTE *nBlue); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_LookupSVGColor(const char *szColor, BYTE *nRed, BYTE *nGreen, BYTE *nBlue); */


/* Pixel access routines */

/* DLL_API BYTE *DLL_CALLCONV FreeImage_GetBits(FIBITMAP *dib); */
HB_FUNC( FI_GETBITS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retptr( FreeImage_GetBits( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BYTE *DLL_CALLCONV FreeImage_GetScanLine(FIBITMAP *dib, int scanline); */
HB_FUNC( FI_GETSCANLINE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_retptr( FreeImage_GetScanLine( dib, hb_parni( 2 ) /* scanline */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   DLL_API BOOL DLL_CALLCONV FreeImage_GetPixelIndex(FIBITMAP *dib, unsigned x, unsigned y, BYTE *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_GetPixelColor(FIBITMAP *dib, unsigned x, unsigned y, RGBQUAD *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_SetPixelIndex(FIBITMAP *dib, unsigned x, unsigned y, BYTE *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_SetPixelColor(FIBITMAP *dib, unsigned x, unsigned y, RGBQUAD *value);
 */

/* DIB info routines */

HB_FUNC( FI_GETCOLORSUSED )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetColorsUsed( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETBPP )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetBPP( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETWIDTH )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetWidth( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETHEIGHT )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetHeight( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETLINE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetLine( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETPITCH )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetPitch( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETDIBSIZE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetDIBSize( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API RGBQUAD *DLL_CALLCONV FreeImage_GetPalette(FIBITMAP *dib); */
HB_FUNC( FI_GETPALETTE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retptr( FreeImage_GetPalette( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETDOTSPERMETERX )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetDotsPerMeterX( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETDOTSPERMETERY )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retnl( FreeImage_GetDotsPerMeterY( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_SetDotsPerMeterX(FIBITMAP *dib, unsigned res); */
HB_FUNC( FI_SETDOTSPERMETERX )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      FreeImage_SetDotsPerMeterX( dib, ( unsigned ) hb_parni( 2 ) /* res */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_SetDotsPerMeterY(FIBITMAP *dib, unsigned res); */
HB_FUNC( FI_SETDOTSPERMETERY )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      FreeImage_SetDotsPerMeterY( dib, ( unsigned ) hb_parni( 2 ) /* res */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BITMAPINFOHEADER *DLL_CALLCONV FreeImage_GetInfoHeader(FIBITMAP *dib); */
HB_FUNC( FI_GETINFOHEADER )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      /* We need not worry about Memory Management - will be automatically released! */
      hb_retptr( FreeImage_GetInfoHeader( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BITMAPINFO *DLL_CALLCONV FreeImage_GetInfo(FIBITMAP *dib); */
HB_FUNC( FI_GETINFO )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retptr( FreeImage_GetInfo( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETCOLORTYPE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetColorType( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETREDMASK )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetRedMask( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETGREENMASK )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetGreenMask( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETBLUEMASK )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetBlueMask( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_GETTRANSPARENCYCOUNT )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retni( FreeImage_GetTransparencyCount( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BYTE * DLL_CALLCONV FreeImage_GetTransparencyTable(FIBITMAP *dib); */
HB_FUNC( FI_GETTRANSPARENCYTABLE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retptr( FreeImage_GetTransparencyTable( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_SetTransparent(FIBITMAP *dib, BOOL enabled); */
HB_FUNC( FI_SETTRANSPARENT )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISLOG( 2 ) )
      FreeImage_SetTransparent( dib, hb_fi_parl( 2 ) /* enabled */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_SetTransparencyTable(FIBITMAP *dib, BYTE *table, int count); */
HB_FUNC( FI_SETTRANSPARENCYTABLE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISPOINTER( 2 ) && HB_ISNUM( 3 ) )
      FreeImage_SetTransparencyTable( dib,
                                      ( BYTE * ) hb_parptr( 2 ) /* table */,
                                      hb_parni( 3 ) /* count */ );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_ISTRANSPARENT )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_fi_retl( FreeImage_IsTransparent( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_HASBACKGROUNDCOLOR )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_fi_retl( FreeImage_HasBackgroundColor( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetBackgroundColor(FIBITMAP *dib, RGBQUAD *bkcolor); */
HB_FUNC( FI_GETBACKGROUNDCOLOR )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
   {
      RGBQUAD bkcolor;

      memset( &bkcolor, 0, sizeof( bkcolor ) );

      hb_fi_retl( FreeImage_GetBackgroundColor( dib, &bkcolor ) );

      hb_storclen( ( char * ) &bkcolor, sizeof( bkcolor ), 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetBackgroundColor(FIBITMAP *dib, RGBQUAD *bkcolor); */
HB_FUNC( FI_SETBACKGROUNDCOLOR )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISCHAR( 2 ) && hb_parclen( 2 ) >= sizeof( RGBQUAD ) )
      hb_fi_retl( FreeImage_SetBackgroundColor( dib,
                                                ( RGBQUAD * ) HB_UNCONST( hb_parc( 2 ) ) /* bkcolor */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ICC profile routines */

/* DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_GetICCProfile(FIBITMAP *dib); */
HB_FUNC( FI_GETICCPROFILE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_retptr( FreeImage_GetICCProfile( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_CreateICCProfile(FIBITMAP *dib, void *data, long size); */
HB_FUNC( FI_CREATEICCPROFILE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISPOINTER( 2 ) && HB_ISNUM( 3 ) )
      hb_retptr( FreeImage_CreateICCProfile( dib,
                                             hb_parptr( 2 ) /* data */,
                                             hb_parnl( 3 ) /* size */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API void DLL_CALLCONV FreeImage_DestroyICCProfile(FIBITMAP *dib); */
HB_FUNC( FI_DESTROYICCPROFILE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      FreeImage_DestroyICCProfile( dib );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Line conversion routines */

/*
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To4(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To4_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To4_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_565_To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_555_To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To24(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To32(BYTE *target, BYTE *source, int width_in_pixels);
 */

/* Smart conversion routines */

HB_FUNC( FI_CONVERTTO4BITS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo4Bits( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTO8BITS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo8Bits( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTOGREYSCALE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertToGreyscale( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTO16BITS555 )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo16Bits555( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTO16BITS565 )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo16Bits565( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTO24BITS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo24Bits( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_CONVERTTO32BITS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertTo32Bits( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantize(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize); */
HB_FUNC( FI_COLORQUANTIZE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_ColorQuantize( dib,
                                                ( FREE_IMAGE_QUANTIZE ) hb_parni( 2 ) ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantizeEx(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize FI_DEFAULT(FIQ_WUQUANT), int PaletteSize FI_DEFAULT(256), int ReserveSize FI_DEFAULT(0), RGBQUAD *ReservePalette FI_DEFAULT(NULL)); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Threshold(FIBITMAP *dib, BYTE T); */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Dither(FIBITMAP *dib, FREE_IMAGE_DITHER algorithm); */
HB_FUNC( FI_DITHER )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_Dither( dib, ( FREE_IMAGE_DITHER ) hb_parni( 2 ) ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertFromRawBits(BYTE *bits, int width, int height, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE)); */
/* DLL_API void DLL_CALLCONV FreeImage_ConvertToRawBits(BYTE *bits, FIBITMAP *dib, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE)); */

HB_FUNC( FI_CONVERTTORGBF )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertToRGBF( dib ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToStandardType(FIBITMAP *src, BOOL scale_linear FI_DEFAULT(TRUE)); */
HB_FUNC( FI_CONVERTTOSTANDARDTYPE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_FIBITMAP_ret( FreeImage_ConvertToStandardType( dib,
                                                        HB_ISLOG( 2 ) ? hb_fi_parl( 2 ) : TRUE /* scale_linear */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToType(FIBITMAP *src, FREE_IMAGE_TYPE dst_type, BOOL scale_linear FI_DEFAULT(TRUE)); */
HB_FUNC( FI_CONVERTTOTYPE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_ConvertToType( dib,
                                                ( FREE_IMAGE_TYPE ) hb_parni( 2 ) /* dst_type */,
                                                HB_ISLOG( 3 ) ? hb_fi_parl( 3 ) : TRUE /* scale_linear */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* tone mapping operators */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ToneMapping(FIBITMAP *dib, FREE_IMAGE_TMO tmo, double first_param FI_DEFAULT(0), double second_param FI_DEFAULT(0)); */
/* DLL_API FIBITMAP* DLL_CALLCONV FreeImage_TmoDrago03(FIBITMAP *src, double gamma FI_DEFAULT(2.2), double exposure FI_DEFAULT(0)); */
/* DLL_API FIBITMAP* DLL_CALLCONV FreeImage_TmoReinhard05(FIBITMAP *src, double intensity FI_DEFAULT(0), double contrast FI_DEFAULT(0)); */

/* ZLib interface */

/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibCompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibUncompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibGZip(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibGUnzip(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibCRC32(DWORD crc, BYTE *source, DWORD source_size); */

/* Metadata routines */

/* tag creation / destruction */
/* DLL_API FITAG *DLL_CALLCONV FreeImage_CreateTag(); */
/* DLL_API void DLL_CALLCONV FreeImage_DeleteTag(FITAG *tag); */
/* DLL_API FITAG *DLL_CALLCONV FreeImage_CloneTag(FITAG *tag); */

/* tag getters and setters */
/* DLL_API const char *DLL_CALLCONV FreeImage_GetTagKey(FITAG *tag); */
/* DLL_API const char *DLL_CALLCONV FreeImage_GetTagDescription(FITAG *tag); */
/* DLL_API WORD DLL_CALLCONV FreeImage_GetTagID(FITAG *tag); */
/* DLL_API FREE_IMAGE_MDTYPE DLL_CALLCONV FreeImage_GetTagType(FITAG *tag); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_GetTagCount(FITAG *tag); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_GetTagLength(FITAG *tag); */
/* DLL_API const void *DLL_CALLCONV FreeImage_GetTagValue(FITAG *tag); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagKey(FITAG *tag, const char *key); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagDescription(FITAG *tag, const char *description); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagID(FITAG *tag, WORD id); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagType(FITAG *tag, FREE_IMAGE_MDTYPE type); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagCount(FITAG *tag, DWORD count); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagLength(FITAG *tag, DWORD length) */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagValue(FITAG *tag, const void *value); */

/* iterator */
/* DLL_API FIMETADATA *DLL_CALLCONV FreeImage_FindFirstMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, FITAG **tag); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_FindNextMetadata(FIMETADATA *mdhandle, FITAG **tag); */
/* DLL_API void DLL_CALLCONV FreeImage_FindCloseMetadata(FIMETADATA *mdhandle); */

/* metadata setter and getter */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, const char *key, FITAG *tag); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_GetMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, const char *key, FITAG **tag); */

/* helpers */
/* DLL_API unsigned DLL_CALLCONV FreeImage_GetMetadataCount(FREE_IMAGE_MDMODEL model, FIBITMAP *dib); */

/* tag to C string conversion */
/* DLL_API const char* DLL_CALLCONV FreeImage_TagToString(FREE_IMAGE_MDMODEL model, FITAG *tag, char *Make FI_DEFAULT(NULL)); */

/* Image manipulation toolkit */

/* rotation and flipping */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateClassic(FIBITMAP *dib, double angle); */
HB_FUNC( FI_ROTATECLASSIC )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_RotateClassic( dib, hb_parnd( 2 ) /* angle */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateEx(FIBITMAP *dib, double angle, double x_shift, double y_shift, double x_origin, double y_origin, BOOL use_mask); */
HB_FUNC( FI_ROTATEEX )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib &&
       HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) && HB_ISNUM( 6 ) && HB_ISLOG( 7 ) )
      hb_FIBITMAP_ret( FreeImage_RotateEx( dib,
                                           hb_parnd( 2 ) /* angle */,
                                           hb_parnd( 3 ) /* x_shift */,
                                           hb_parnd( 4 ) /* y_shift */,
                                           hb_parnd( 5 ) /* x_origin */,
                                           hb_parnd( 6 ) /* y_origin */,
                                           hb_fi_parl( 7 ) /* use_mask */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_FLIPHORIZONTAL )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_fi_retl( FreeImage_FlipHorizontal( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_FLIPVERTICAL )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_fi_retl( FreeImage_FlipVertical( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_JPEGTransform(const char *src_file, const char *dst_file, FREE_IMAGE_JPEG_OPERATION operation, BOOL perfect FI_DEFAULT(FALSE)); */

/* upsampling / downsampling */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Rescale(FIBITMAP *dib, int dst_width, int dst_height, FREE_IMAGE_FILTER filter); */
HB_FUNC( FI_RESCALE )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) )
      hb_FIBITMAP_ret( FreeImage_Rescale( dib,
                                          hb_parni( 2 ) /* dst_width */,
                                          hb_parni( 3 ) /* dst_height */,
                                          ( FREE_IMAGE_FILTER ) hb_parni( 4 ) ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* color manipulation routines (point operations) */

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustCurve(FIBITMAP *dib, BYTE *LUT, FREE_IMAGE_COLOR_CHANNEL channel); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustGamma(FIBITMAP *dib, double gamma); */
HB_FUNC( FI_ADJUSTGAMMA )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_fi_retl( FreeImage_AdjustGamma( dib, hb_parnd( 2 ) /* gamma */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustBrightness(FIBITMAP *dib, double percentage); */
HB_FUNC( FI_ADJUSTBRIGHTNESS )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_fi_retl( FreeImage_AdjustBrightness( dib, hb_parnd( 2 ) /* percentage */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustContrast(FIBITMAP *dib, double percentage); */
HB_FUNC( FI_ADJUSTCONTRAST )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_fi_retl( FreeImage_AdjustContrast( dib, hb_parnd( 2 ) /* percentage */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( FI_INVERT )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib )
      hb_fi_retl( FreeImage_Invert( dib ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetHistogram(FIBITMAP *dib, DWORD *histo, FREE_IMAGE_COLOR_CHANNEL channel FI_DEFAULT(FICC_BLACK)); */

/* channel processing routines */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_GetChannel(FIBITMAP *dib, FREE_IMAGE_COLOR_CHANNEL channel); */
HB_FUNC( FI_GETCHANNEL )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) )
      hb_FIBITMAP_ret( FreeImage_GetChannel( dib,
                                             ( FREE_IMAGE_COLOR_CHANNEL ) hb_parni( 2 ) ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetChannel(FIBITMAP *dib, FIBITMAP *dib8, FREE_IMAGE_COLOR_CHANNEL channel); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_GetComplexChannel(FIBITMAP *src, FREE_IMAGE_COLOR_CHANNEL channel); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetComplexChannel(FIBITMAP *dst, FIBITMAP *src, FREE_IMAGE_COLOR_CHANNEL channel); */

/* copy / paste / composite routines */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Copy(FIBITMAP *dib, int left, int top, int right, int bottom); */
HB_FUNC( FI_COPY )
{
   FIBITMAP * dib = hb_FIBITMAP_par( 1 );

   if( dib && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
      hb_FIBITMAP_ret( FreeImage_Copy( dib,
                                       hb_parni( 2 ) /* left */,
                                       hb_parni( 3 ) /* top */,
                                       hb_parni( 4 ) /* right */,
                                       hb_parni( 5 ) /* bottom */ ), HB_TRUE );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_Paste(FIBITMAP *dst, FIBITMAP *src, int left, int top, int alpha); */
HB_FUNC( FI_PASTE )
{
   FIBITMAP * dst = hb_FIBITMAP_par( 1 );
   FIBITMAP * src = hb_FIBITMAP_par( 2 );

   if( dst && src && HB_ISNUM( 3 ) && HB_ISNUM( 4 ) && HB_ISNUM( 5 ) )
      hb_fi_retl( FreeImage_Paste( dst, src,
                                   hb_parni( 3 ) /* left */,
                                   hb_parni( 4 ) /* top */,
                                   hb_parni( 5 ) /* alpha */ ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Composite(FIBITMAP *fg, BOOL useFileBkg FI_DEFAULT(FALSE), RGBQUAD *appBkColor FI_DEFAULT(NULL), FIBITMAP *bg FI_DEFAULT(NULL)); */
