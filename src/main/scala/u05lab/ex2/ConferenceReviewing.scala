package u05lab.ex2

import u05lab.ex2.ConferenceReviewing.Question

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:

  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    private var reviews: List[(Int, Map[Question, Int])] = Nil

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
      this.reviews = this.reviews ::: List((article, scores))

    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      this.reviews = this.reviews ::: List((article, Map(Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin)))

    override def orderedScores(article: Int, question: Question): List[Int] =
      this.reviews.collect({case (a, m) if a == article => m(question)}).sorted

    override def averageFinalScore(article: Int): Double =
      //this.reviews.collect({case (a, m) if a == article => m(Question.FINAL)}).sum / this.reviews.count(_._1 == article).toDouble
      this.reviews.filter(_._1 == article).map(_._2(Question.FINAL)).sum / this.reviews.count(_._1 == article).toDouble

    import ImplementationHelpers.*

    override def acceptedArticles: Set[Int] =
      this.reviews.map(_._1).distinct.filter(accepted).toSet

    override def sortedAcceptedArticles: List[(Int, Double)] =
      this.acceptedArticles.map(x => (x, averageFinalScore(x))).toList.sortBy((_, v) => v)

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      this.reviews.map(_._1).distinct.map(x => x -> averageWeightedFinalScore(x)).toMap

    private object ImplementationHelpers:
      def accepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 && reviews.filter(_._1 == article).map(_._2).count(_(Question.RELEVANCE) >= 8.0) != 0

      def averageWeightedFinalScore(article: Int): Double =
      //reviews.filter(_._1 == article).map[Double](x => x._2(Question.FINAL) * (x._2(Question.CONFIDENCE) / 10.0)).sum / reviews.count(_._1 == article).toDouble
        reviews.collect({case (a, m) if a == article => m(Question.FINAL) * (m(Question.CONFIDENCE) / 10.0)}).sum / reviews.count(_._1 == article).toDouble