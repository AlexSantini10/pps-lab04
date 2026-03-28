package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*
import it.unibo.pps.tasks.adts.SchoolModel.*
import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*

class SchoolModelTest:

  val schoolModule: SchoolModule = BasicSchoolModule
  import schoolModule.*

  @Test def emptySchoolHasNoTeachers() =
    val school = emptySchool
    assertEquals(Nil(), school.teachers)

  @Test def emptySchoolHasNoCourses() =
    val school = emptySchool
    assertEquals(Nil(), school.courses)

  @Test def emptySchoolHasNoTeacherByName() =
    val school = emptySchool
    assertFalse(school.hasTeacher("John"))

  @Test def emptySchoolHasNoCourseByName() =
    val school = emptySchool
    assertFalse(school.hasCourse("Math"))

  @Test def assigningTeacherAddsTeacher() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertEquals(Cons("John", Nil()), school.teachers)

  @Test def assigningTeacherAddsCourse() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertEquals(Cons("Math", Nil()), school.courses)

  @Test def assigningTeacherMakesTeacherPresent() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertTrue(school.hasTeacher("John"))

  @Test def assigningTeacherMakesCoursePresent() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertTrue(school.hasCourse("Math"))

  @Test def assigningTwoCoursesDoesNotDuplicateTeacher() =
    val john = teacher("John")

    val school = emptySchool
      .setTeacherToCourse(john, course("Math"))
      .setTeacherToCourse(john, course("Italian"))

    assertEquals(Cons("John", Nil()), school.teachers)

  @Test def assigningSameCourseToDifferentTeachersDoesNotDuplicateCourse() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))
      .setTeacherToCourse(teacher("Mary"), course("Math"))

    assertEquals(Cons("Math", Nil()), school.courses)

  @Test def coursesOfTeacherWithOneCourse() =
    val john = teacher("John")
    val math = course("Math")

    val school = emptySchool
      .setTeacherToCourse(john, math)

    assertEquals(Cons(math, Nil()), school.coursesOfATeacher(john))

  @Test def coursesOfTeacherWithMultipleCourses() =
    val john = teacher("John")
    val math = course("Math")
    val italian = course("Italian")

    val school = emptySchool
      .setTeacherToCourse(john, math)
      .setTeacherToCourse(john, italian)

    assertEquals(
      Cons(math, Cons(italian, Nil())),
      school.coursesOfATeacher(john)
    )

  @Test def coursesOfTeacherWithoutCoursesIsEmpty() =
    val john = teacher("John")
    val school = emptySchool

    assertEquals(Nil(), school.coursesOfATeacher(john))

  @Test def hasTeacherReturnsFalseForUnknownTeacher() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertFalse(school.hasTeacher("Mary"))

  @Test def hasCourseReturnsFalseForUnknownCourse() =
    val school = emptySchool
      .setTeacherToCourse(teacher("John"), course("Math"))

    assertFalse(school.hasCourse("Italian"))

  @Test def setTeacherToCourseDoesNotMutateOriginalSchool() =
    val original = emptySchool
    val updated = original.setTeacherToCourse(teacher("John"), course("Math"))

    assertEquals(Nil(), original.teachers)