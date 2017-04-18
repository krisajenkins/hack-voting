module Slamdown (renderBlock) where

import Data.Array (fromFoldable)
import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Halogen.HTML (HTML, a, blockquote_, br_, code_, em_, h1_, h2_, h3_, h4_, h5_, h6_, hr_, img, li_, ol_, p_, pre_, strong_, text, ul_)
import Halogen.HTML.Properties (alt, href, id_, src)
import Halogen.HTML.Properties as HP
import Prelude
import Text.Markdown.SlamDown (Block(..), Inline(..), LinkTarget(..), ListType(..))

renderInline :: forall t102 t103 t130 t99. (Applicative t99) => Inline t130 -> t99 (HTML t103 t102)
renderInline i =
  case i of
    Str s → pure $ text s
    Entity s → pure $ text s
    Space → pure $ text " "
    SoftBreak → pure $ text "\n"
    LineBreak → pure $ br_
    Emph is → em_ <$> traverse renderInline (fromFoldable is)
    Strong is → strong_ <$> traverse renderInline (fromFoldable is)
    Code _ c → pure $ code_ [ text c ]
    Link body tgt → do
      let
        href (InlineLink url) = url
        href (ReferenceLink tgt') = maybe "" ("#" <> _) tgt'
      a [ HP.href $ href tgt ] <$> traverse renderInline (fromFoldable body)
    Image body url →
      pure $ img
        [ src url
        , alt $ foldMap stripInline body
        ]
    FormField label required field →
      pure $ text ""

h_ :: forall p a. Int → Array (HTML p a) → HTML p a
h_ 1 = h1_
h_ 2 = h2_
h_ 3 = h3_
h_ 4 = h4_
h_ 5 = h5_
h_ _ = h6_

el_ :: forall i p. ListType -> Array (HTML p i) -> HTML p i
el_ (Bullet _)  = ul_
el_ (Ordered _) = ol_


stripInline ∷ forall v. Inline v → String
stripInline i =
  case i of
    Str s → s
    Entity s → s
    Space → " "
    SoftBreak → "\n"
    LineBreak → "\n"
    Emph is → foldMap stripInline is
    Strong is → foldMap stripInline is
    Code _ c → c
    Link body _ → foldMap stripInline body
    _ → ""

renderBlock ::
  forall v p m i.
  Monad m => Block v -> m (HTML p i)
renderBlock b =
  case b of
    Paragraph is →
      p_ <$> traverse renderInline (fromFoldable is)
    Header lvl is →
      h_ lvl <$> traverse renderInline (fromFoldable is)
    Blockquote bs →
      blockquote_ <$> traverse renderBlock (fromFoldable bs)
    Lst lt bss → do
      let
        item bs = li_ <$> traverse renderBlock (fromFoldable bs)
      el_ lt <$> traverse item (fromFoldable bss)
    CodeBlock _ ss →
      pure $ pre_ [ code_ [ text (joinWith "\n" $ fromFoldable ss) ] ]
    LinkReference l url →
      pure $ p_
        [ text (l <> ": ")
        , a [ id_ l, href url ] [ text url ]
        ]
    Rule →
      pure hr_

-- renderBlocks ∷ ∀ p. List (Block v) → Fresh (Array (HTML p (SlamDownQuery v)))
-- renderBlocks = go [] Nil
--       where
--       go html fs bs =
--         case bs of
--           L.Cons (SD.Paragraph (L.Cons (SD.FormField label required field) L.Nil)) bs' →
--             go html (L.Cons { label, required, field } fs) bs'
--           L.Cons b bs' → do
--             bHtml ← renderBlock b
--             if L.null fs
--               then go (html <> pure bHtml) L.Nil bs'
--               else do
--                 fsHtml ← renderFormSet (L.reverse fs)
--                 go (html <> pure fsHtml <> pure bHtml) L.Nil bs'
--           L.Nil →
--             if L.null fs
--               then pure html
--               else do
--                 fsHtml ← renderFormSet (L.reverse fs)
--                 pure (html <> pure fsHtml)
