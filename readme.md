# Fluent LiveBindings

Fluent LiveBindings is a simple library to make it easy to add **standard** LiveBindings in code. 

## Background

Visual Livebindings were introduced into Delphi and C++Builder nearly 10 years ago as a way for both VCL and FMX apps to bind data to visual controls. Since then, it’s been steadily improved, including a bunch of performance improvements in 10.4. http://docwiki.embarcadero.com/RADStudio/Sydney/en/What%27s_New#Key_Run-Time_Library_Enhancements

However, for all that time, they’ve focussed very much on the Visual part of the name. That’s fine for some situations, but it has bothered me over the years that there’s no easy way to see all of the details of a Form’s bindings in a single place, without endlessly clicking through the Livebindings Designer and the Object Inspector. You can create them in code, sure, but the object model makes it very unreadable and time consuming.

After grumbling about this for years, I finally decided to do something more useful about it. Enter Fluent LiveBindings: a simple, open source library that provides an easy, readable way to create standard Livebindings in code.

## How to use Fluent LiveBindings

On a form that already has a TBindingsList, add LiveBindings.Fluent to the uses clause. Then you can write code like this to bind an edit box to a label:

```delphi
BindingsList1.BindComponent(Edit2).ToComponent(Label2, 'Text');
````

The example above is a FMX Label, hence the Text property. For VCL, It's the same but you'd swap 'Text' with 'Caption'.

If you want the Edit box to Track changes, add the call to Track.
```delphi
BindingsList1.BindComponent(Edit2).Track.ToComponent(Label2, 'Text');
````

If you want to format it before showing it in the Edit box, add the Format call:
```delphi
BindingsList1.BindComponent(Edit2).Format('"Foo " + %s').ToComponent(Label2, 'Text');
````

You can also control the direction:
```delphi
BindingsList1.BindComponent(Edit2).ToComponent(Label2, 'Text').BiDirectional;
````

Interestingly, you can also bind directly between two Edit boxes, something the LiveBindings designer won't let you do:
```delphi
BindingsList1.BindComponent(Edit2).ToComponent(Edit3, 'Text').BiDirectional;
````


Have a look in the example project for more, errr, examples. 

## Disclaimers
First, this is just about creating the bindings. Once created, they behave the same as if you had created them visually. There's no overhead added once you've created them. Equally, if you dislike LiveBindings for some reason other than how hard they are to create in code, this won't solve that for you. 

Second, this started as an experiment. I've used most of them across multiple projects, so they should be reasonably stable. The one exception is the Expression Targets/Sources. I've marked them as experimental in the code as I'm still shaking some issues out. Don't rely on those. 

Last, if they don't do quite what you want, extend them and send me a pull request. 
