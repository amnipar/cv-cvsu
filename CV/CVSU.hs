{-#LANGUAGE FlexibleInstances #-}
module CV.CVSU
( readPixelImage
, writePixelImage
) where

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc

import CVSU.PixelImage as CVSU hiding (readPixelImage, writePixelImage)
import CVSU.C.Types
import CV.Bindings.CVSU
import CV.Image

import Control.Monad

fromIplImage :: PixelFormat -> Ptr () -> IO (PixelImage)
fromIplImage format ptr = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg -> do
    result <- c'pixel_image_create_from_ipl_image pimg (castPtr ptr) (cPixelFormat format)
    if result /= resultSuccess
       then error $ "fromIplImage failed with " ++ (show result)
       else ptrToPixelImage fimg

toIplImage :: PixelFormat -> PixelImage -> IO (Ptr (CIplImage))
toIplImage format img = do
  withForeignPtr (imagePtr img) $ \pimg -> do
    let
      mallocIplImage :: IO (Ptr (Ptr CIplImage))
      mallocIplImage = malloc
    ppipl <- mallocIplImage
    result <- c'ipl_image_create_from_pixel_image ppipl pimg (cPixelFormat format)
    if result /= resultSuccess
       then error "Creating IplImage failed"
       else do
         pipl <- peek ppipl
         return pipl

toBareImage :: IO (Ptr CIplImage) -> IO (Ptr BareImage)
toBareImage = liftM castPtr

instance CVSU.Convertible (Image GrayScale D8) where
  fromPixelImage img = creatingImage $ toBareImage $ toIplImage FormatGrey img
  toPixelImage img = do
    withGenImage img $ \pimg ->
      fromIplImage FormatGrey (castPtr pimg)

instance CVSU.Convertible (Image RGB D8) where
  fromPixelImage pimg = creatingImage $ toBareImage $ toIplImage FormatRGB pimg
  toPixelImage img = do
    withGenImage img $ \pimg ->
      fromIplImage FormatRGB (castPtr pimg)

readPixelImage :: String -> IO (PixelImage)
readPixelImage f = do
  fimg <- allocPixelImage
  withForeignPtr fimg $ \pimg ->
    withCString f $ \c_str -> do
      result <- c'pixel_image_create_from_file pimg c_str typeByte formatGrey
      if result /= resultSuccess
        then return NullImage
        else ptrToPixelImage fimg

writePixelImage :: String -> PixelImage -> IO ()
writePixelImage filename img =
  withForeignPtr (imagePtr img) $ \pimg ->
    withCString filename $ \pfilename -> do
      result <- c'pixel_image_write_to_file pimg pfilename
      if result /= resultSuccess
         then error $ "writePixelImage failed with " ++ (show result)
         else return ()
