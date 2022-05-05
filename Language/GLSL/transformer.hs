newtype MaybeTT m a = MaybeTT {runMaybeTT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeTT m) where
  return = MaybeTT . return . Just

  x >>= f = MaybeTT $ do
    v <- runMaybeTT x
    case v of
      Nothing -> return Nothing
      Just y -> runMaybeTT (f y)

-- fail _ = MaybeTT (return Nothing)

mapMaybeTT :: (m (Maybe a) -> n (Maybe b)) -> MaybeTT m a -> MaybeTT n b
mapMaybeTT f = MaybeTT . f . runMaybeTT

instance (Functor m) => Functor (MaybeTT m) where
  fmap f = mapMaybeTT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeTT m) where
  pure = MaybeTT . return . Just

  mf <*> mx = MaybeTT $ do
    mb_f <- runMaybeTT mf
    case mb_f of
      Nothing -> return Nothing
      Just f -> do
        mb_x <- runMaybeTT mx
        case mb_x of
          Nothing -> return Nothing
          Just x -> return (Just (f x))

  m *> k = m >>= \_ -> k